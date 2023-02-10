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
      type == 'Hourly Q' ~ 'flow_hrly',
      type == 'Daily Q' ~ 'flow_dv',
      TRUE ~ NA_character_
    )) %>% rename(value = 'Q',
                  station_nm = 'Stream') %>%
    select(-type) %>% filter(!is.na(value))

  gd_q <- gd_q %>% left_join(gpt %>% select(station_nm, sid)) %>%
          select(-station_nm)

  gd_tss <- gd %>% select(Stream, dt, TSS, type) %>%
    mutate(param = case_when(
      type == 'Observed Q' ~ 'tss_obs',
      type == 'Hourly Q' ~ 'tss_hrly',
      type == 'Daily Q' ~ 'tss_dv',
      TRUE ~ NA_character_
    )) %>% rename(value = 'TSS',
                  station_nm = 'Stream') %>%
    select(-type) %>% filter(!is.na(value))

  gd_tss <- gd_tss %>% left_join(gpt %>% select(station_nm, sid)) %>%
    select(-station_nm)
iv <- gd_q %>% filter(param == 'flow_hrly') %>% select(-param)

dv <- gd_q %>% filter(param == 'flow_dv') %>% select(-param)
obs <- gd_q %>% filter(param == 'flow_obs') %>% select(-param)
obs_tss <- gd_tss %>% select(-param)
names(mydb)

dbWriteTable(mydb, "station_metadata", gpt)
dbWriteTable(mydb, "flow_obs", obs, overwrite = T)
dbWriteTable(mydb, "precip_obs", value = data.frame(dt = vector('character'),
                                                value = vector('numeric'),
                                                sid = vector('integer')))

dbRemoveTable(mydb, 'stream_tss')

dbListTables(mydb)

dbGetQuery(mydb, 'SELECT * FROM flow_obs WHERE sid IN (19;17;20) LIMIT 255')

explain(testing_q %>% filter(sid %in% c(1:5)))

library(dbplyr)

mydb
testing_q <- tbl(mydb,'flow_dv')

t <- collect(testing_q)

m <- tbl(mydb, 'station_metadata')

dbGetQuery(mydb, "ALTER TABLE station_metadata ADD COLUMN district TEXT")

dbExecute(mydb, "UPDATE station_metadata SET district = :district where sid = :sid",
          params=data.frame(district = 'Ksanka',
                            sid = m_collect$sid))


m_collect <- collect(m)

t_join <- t %>% left_join(m_collect)

young <- testing_q %>%
             filter(station_nm == 'Young')

collect(young)


plot(nhdplusTools::get_nldi_basin(list(featureSource = 'comid',
                                  featureID = '22878867')))$geometry
