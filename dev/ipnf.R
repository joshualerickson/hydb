library(tidyverse)
library(sf)
devtools::document()

# intial metadata - this will likely be changing over time
path <- r"{C:/Users/joshualerickson/USDA/Northern Region Hydrology - Documents/data-madness/forests/IPNF/wtemp/iv/}"
md <- readxl::read_xlsx(paste0(path, '/Idaho Panhandle National Forest - North Zone/Idaho Panhandle National Forest - North Zone/2022 Integrated Report - C4D Sample Location Template.xlsx'), sheet = 2) %>%
  janitor::clean_names()

md

md_sf_point <- md %>%
  mutate(comments = paste0('Tribal Land: ', tribal_land_y_n)) %>%
  st_as_sf(coords = c('station_longitude', 'station_latitude'), crs = 4326)%>%
  select(station_nm = station_name,
         purpose = parameters_measured,
         comments
  )

# check overlapping sites
# this checks for duplicates and then collapse with character columns
md_sf_point_clean <- hydb_clean_duplicate_metadata(md_sf_point)

# now run metadata function

md_df <- hydb_setup_metadata(md_sf_point_clean)

# for appending
mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'station_metadata', md_df)

hydb_disconnect()

# bulk load data into hydb
# when you have the data in a format where you can easily bulk then then it's recommended to do that

# read in the metadata to give an sid to the data
stations <- fetch_hydb('station_metadata')

# we'll enter in the stream temp data from excel files

stations_ipnf <- stations %>% filter(str_detect(forest,'Idaho'))

# now read in all of north zones excel files

stations_ipnf_nz <- stations_ipnf %>% filter(district != 'St. Joe Ranger District')

path <- r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\IPNF\wtemp\iv\Idaho Panhandle National Forest - North Zone\Idaho Panhandle National Forest - North Zone\Temperature Data}'


files <- list.files(paste0(path))



find_closest_match <- function(text, keys) {

  cleaned_text <- gsub("[^a-zA-Z]", " ", gsub("_", " ", text))

  distances <- stringdist::stringdist(cleaned_text, keys, method = 'jw')

  closest_match <- keys[which.min(distances)]

}

station_together <- tibble()

for(i in files) {

station_name <- find_closest_match(i, unique(stations_ipnf_nz$station_nm))

station <- readxl::read_xlsx(paste0(path, '\\', i), skip = 1) %>%
           janitor::clean_names() %>%
           dplyr::select(-1) %>%
           dplyr::rename(dt = 1,
                         iv_00011 = 2) %>%
           dplyr::mutate(dplyr::across(c(2), ~.x*1.8 + 32))%>%
           mutate(iv_00011 = ifelse(iv_00011 < 0 , 0, iv_00011)) %>%
          mutate(station_nm = station_name)

station_together <- dplyr::bind_rows(station_together, station)

}

# now combine the files

wtemp_ipnf <- station_together %>% left_join(stations_ipnf_nz %>% select(sid, station_nm)) %>% select(-station_nm)

wtemp_ipnf  %>%
ggplot(aes(dt, iv_00011, group = sid)) +
  geom_line() +
  facet_wrap(~sid, scales = 'free')

# for appending
mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'wtemp_iv', wtemp_ipnf)

# now create daily's from the ipnf data and south zone ipnf

wtemp_ipnf_dv <- hydb_daily_transform(wtemp_ipnf) %>% filter(statistic_type_code != '00006')

wtemp_ipnf_dv  %>%
  ggplot(aes(date, dv_00011, group = sid)) +
  geom_line() +
  facet_wrap(statistic_type_code~sid, scales = 'free')

DBI::dbAppendTable(mydb, 'wtemp_dv', wtemp_ipnf_dv)

south_zone <- fetch_hydb('wtemp_iv', tbl_only = T)

south_zone <- south_zone %>% filter(substring(sid, 1, 6) == '010404') %>% collect() %>% mutate(dt = as_datetime(dt))

south_zone_dv <- hydb_daily_transform(south_zone) %>% filter(statistic_type_code != '00006')


DBI::dbAppendTable(mydb, 'wtemp_dv', south_zone_dv)

hydb_disconnect()


# now get the daily's and write to csv files in ipnf folder


ipnf_wtemp_dv <- fetch_hydb('wtemp_dv', tbl_only = T)

ipnf_wtemp_dv <- ipnf_wtemp_dv %>%
  filter(substring(sid, 1, 4) == '0104') %>%
  collect() %>%
  mutate(date = as_date(date))

ipnf_wtemp_dv_pivot <- ipnf_wtemp_dv %>%
  mutate(statistic_type_code = hydb:::stat_cd_to_name(statistic_type_code)) %>%
  pivot_wider(values_from = dv_00011, names_from = statistic_type_code)

ipnf_wtemp_dv_pivot_gp <-
  ipnf_wtemp_dv_pivot %>%
  group_nest(sid, year = year(date))


stations_ipnf

path_dv <- r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\IPNF\wtemp\final\dv}'

for(i in seq_len(nrow(ipnf_wtemp_dv_pivot_gp))){

  station_id <- ipnf_wtemp_dv_pivot_gp$sid[i]
  year <- ipnf_wtemp_dv_pivot_gp$year[i]
  data <- ipnf_wtemp_dv_pivot_gp$data[[i]]

  station_name <- stations %>% dplyr::filter(sid == station_id) %>% pull(station_nm)

  year_dir <- file.path(path_dv, year)

  if(!dir.exists(year_dir)) {
    dir.create(year_dir, recursive = T)
  }

    station_dir <- file.path(year_dir, paste0(station_name, '_', station_id))

    if(!dir.exists(station_dir)) {
      dir.create(station_dir, recursive = T)
    }

    csv_file <- file.path(station_dir, paste0(station_name, '_', year, '_dv_', station_id, '.csv'))
    write_csv(data, csv_file)

}




path_md <- r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\IPNF\wtemp\station_metadata.csv}'

write_csv(stations %>% filter(forest == 'Idaho Panhandle National Forests',
                              str_detect(purpose, 'Temperature')), path_md)




ipnf_wtemp_dv_pivot_slide <- ipnf_wtemp_dv_pivot %>%
  group_by(sid, year = year(date)) %>%
  mutate(across(max:median, ~slider::slide_index_dbl(.,.i = date, ~mean(.x, na.rm = T), .after = 7), .names = "{col}_7day_mean"))

ipnf_wtemp_dv_pivot_slide %>%
  ggplot(aes(date, max_7day_mean)) +
  geom_line() +
  facet_wrap(~sid, scales = 'free')
