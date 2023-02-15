library(DBI)
library(RSQLite)
library(tidyverse)

path <- 'T:/FS/NFS/Kootenai/Program/2500Watershed/GIS/SO/hydb'

mydb <- dbConnect(RSQLite::SQLite(), paste0(path,"/hydb.sqlite"))

dbListTables(mydb)

gd <- feather::read_feather('D:/documents/Project File/gage_stations/district_gaging/district_gaging/gaging_data.feather')

gpt <- read_csv('D:/documents/Project File/gage_stations/district_gaging/district_gaging/gaging_points.csv')
gpt <- gpt %>% group_by(Stream) %>% slice(n=1) %>% ungroup()


  #get comids for nwis sites
  comids <- function(point) {
    clat <- point$geometry[[1]][[2]]
    clng <- point$geometry[[1]][[1]]

    ids <- paste0("https://labs.waterdata.usgs.gov/api/nldi/linked-data/comid/position?coords=POINT%28",
                  clng,"%20", clat, "%29")

    error_ids <- httr::GET(url = ids,
                           httr::write_disk(path = file.path(tempdir(),
                                                             "nld_tmp.json"),overwrite = TRUE))

    nld <- jsonlite::fromJSON(file.path(tempdir(),"nld_tmp.json"))
  }

  gpt <- gpt %>% rename(station_nm = 'Stream')

  comids_nwis <- gpt %>%
    sf::st_as_sf(coords = c('Long', 'Lat'), crs = 4326) %>%
    split(.$station_nm) %>%
    furrr::future_map(~comids(.)$features$properties$identifier)

  comids_nwis <- tibble(COMID = as.numeric(comids_nwis),
                        station_nm = names(comids_nwis))
  gpt <- gpt %>% left_join(comids_nwis)

  gpt <- gpt %>% mutate(sid = row_number())

  gd_q <- gd %>% select(Stream, dt, Q, type) %>%
    mutate(param = case_when(
      type == 'Observed Q' ~ 'flow_obs',
      type == 'Hourly Q' ~ 'flow_iv',
      type == 'Daily Q' ~ 'flow_dv',
      TRUE ~ NA_character_
    )) %>% rename(value = 'Q',
                  station_nm = 'Stream') %>%
    select(-type) %>% filter(!is.na(value)) %>%
    mutate(date = lubridate::date(dt),
           time = NA_real_)

  meta_data <- dplyr::collect(tbl(mydb, 'station_metadata'))
  random_numbers <- sample(x = 10000:99999,
             size = 20,
             replace = FALSE)
  meta_data <- meta_data %>% mutate(sid = paste0('011401', random_numbers))
  meta_data <- meta_data %>% mutate(region = "Northern Region")
  meta_data <- meta_data %>% relocate(region, .after = sid)

  gd_q <- gd_q %>% left_join(meta_data %>% select(station_nm, sid)) %>%
          select(-station_nm)

  gd_tss <- gd %>% select(Stream, dt, TSS, type) %>%
    mutate(param = case_when(
      type == 'Observed Q' ~ 'tss_obs',
      type == 'Hourly Q' ~ 'tss_iv',
      type == 'Daily Q' ~ 'tss_dv',
      TRUE ~ NA_character_
    )) %>% rename(value = 'TSS',
                  station_nm = 'Stream') %>%
    select(-type) %>% filter(!is.na(value))%>%
    mutate(date = lubridate::date(dt),
           time = NA_real_)

  gd_tss <- gd_tss %>% left_join(meta_data %>% select(station_nm, sid)) %>%
    select(-station_nm)

#meta data entry

dbWriteTable(mydb, "station_metadata", meta_data, overwrite = T)

# flow section
iv <- gd_q %>% filter(param == 'flow_iv') %>% select(dt, iv_00060 = 'value', sid)

dv <- gd_q %>% filter(param == 'flow_dv') %>% select(date, dv_00060 = 'value',sid)
obs <- gd_q %>% filter(param == 'flow_obs') %>% select(date, time, obs_00060 = 'value', sid)


dbWriteTable(mydb, "flow_obs", obs, overwrite = T)
#tss section
iv <- gd_tss %>% filter(param == 'tss_iv') %>% select(dt, iv_00530 = 'value', sid)

dv <- gd_tss %>% filter(param == 'tss_dv') %>% select(date, dv_00530 = 'value',sid)
obs <- gd_tss %>% filter(param == 'tss_obs') %>% select(date, time, obs_00530 = 'value', sid)


dbWriteTable(mydb, "tss_dv", dv, overwrite = T)


dbWriteTable(mydb, "airtemp_obs", value = data.frame(date = vector('character'),
                                                     time = vector('numeric'),
                                                obs_00021 = vector('numeric'),
                                                sid = vector('integer')),
             overwrite = TRUE)



dbRemoveTable(mydb, 'stream_tss')

dbListTables(mydb)

dbGetQuery(mydb, 'SELECT * FROM flow_obs WHERE sid IN (19;17;20) LIMIT 255')

explain(testing_q %>% filter(sid %in% c(1:5)))

library(dbplyr)


m <- tbl(mydb, 'station_metadata')

dbGetQuery(mydb, "ALTER TABLE station_metadata ADD COLUMN district TEXT")

dbExecute(mydb, "UPDATE station_metadata SET district = :district where sid = :sid",
          params=data.frame(district = 'Ksanka',
                            sid = m_collect$sid))


m <- tbl(mydb, 'stage_obs')
m_collect <- collect(m)

t_join <- t %>% left_join(m_collect)

young <- testing_q %>%
             filter(station_nm == 'Young')

collect(young)


plot(nhdplusTools::get_nldi_basin(list(featureSource = 'comid',
                                  featureID = '22878867')))$geometry
