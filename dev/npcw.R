devtools::document()

md <- fetch_hydb('metadata')

library(DBI)
library(RSQLite)
library(tidyverse)

path <- 'T:/FS/NFS/Kootenai/Program/2500Watershed/GIS/SO/hydb'

mydb <- dbConnect(RSQLite::SQLite(), paste0(path,"/hydb.sqlite"))

dbListTables(mydb)

#### Function to get COMID for site
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

#### Get COMID with meta data long, lat
#### Bring in NPCW gauging sites
path = r"{T:\FS\NFS\NezPerceClearwater\Program\2500WatershedAirMgmt\GIS\MONITORING\ForestMonitoring_H20.gdb}"
library(sf)
meta_data <- read_sf(path, layer = 'Gauge_NPCLW')

# check to make sure there are no double named rows
length(unique(meta_data$Station)) == nrow(meta_data)

# now give each station its sid
# read in the admin districts
admin_districts <- read_sf('Z:/simple_features/lands/admin_units_district.shp')%>%
  st_transform(4326) %>% st_make_valid()

meta_data <- meta_data %>%
  st_transform(4326) %>%
  st_make_valid()

meta_data2 <- meta_data %>% st_intersection(admin_districts %>% select(DISTRICTOR, FORESTNAME, DISTRICTNA)) %>% mutate(sid = DISTRICTOR)

site_off_fs_land <- meta_data[!meta_data$Station %in% meta_data2$Station,]
site_off_fs_land$sid <- '011752'

meta_data <- bind_rows(meta_data2, site_off_fs_land)


meta_data_coordinates <- meta_data %>% bind_cols(tibble(Long = st_coordinates(.)[,1],Lat = st_coordinates(.)[,2])) %>% st_drop_geometry()
meta_data <- meta_data %>% bind_cols(meta_data_coordinates %>% select(Long, Lat))


#now add unique 5-digit code
add_to_sid <- as.character(c(paste0('0000', seq(1,9,1)), paste0('000', seq(10,52,1))))
meta_data <- meta_data %>% arrange(desc(Lat)) %>%
  mutate(sid = paste0(sid, add_to_sid))

mapview::mapview(meta_data, zcol = 'sid')

#then run to get COMID
comids_nwis <- meta_data %>% rename(geometry = 'Shape') %>%
  split(.$sid) %>%
  furrr::future_map(~comids(.)$features$properties$identifier)

comids_nwis <- tibble(COMID = as.character(comids_nwis),
                      sid = names(comids_nwis))

meta_data <- meta_data %>% select(station_nm = Station,
                                  forest = FORESTNAME,
                                  district = DISTRICTNA,
                                  Long, Lat, sid) %>% left_join(comids_nwis)

#check and make sure everything is good
basins = list()
for(i in meta_data$COMID){
blist <- nhdplusTools::get_nldi_basin(list(featureSource = 'comid',
                                  featureID = i))

basins <- append(basins, list(blist))
}

basins_sf <- bind_rows(basins)

mapview::mapview(basins_sf) + st_as_sf(md, coords = c('Long', 'Lat'), crs = 4326) %>%
  mapview::mapview(zcol = 'sid')

meta_data[meta_data$sid == '01175200009',]$forest = meta_data[10,]$forest
meta_data[meta_data$sid == '01175200009',]$district = meta_data[10,]$district
### now drop geometry

meta_data_final <- meta_data %>% st_drop_geometry()
meta_data_final <- meta_data_final %>% mutate(region = 'Northern Region')
#meta data entry
# meta data sid should follow the pattern;region,forest,district,6-digit #

dbWriteTable(mydb, "station_metadata", meta_data, overwrite = T)


# for appending
dbAppendTable(mydb, 'station_metadata', meta_data_final)

# for deleting mess-ups
#dbExecute(mydb, 'DELETE FROM station_metadata WHERE "forest" == "Nez Perce-Clearwater National Forest"')

#### for creating new tables

#dv tables
dbWriteTable(mydb, "wtemp_dv", value = data.frame(date = vector('numeric'),
                                                  dv_00011 = vector('numeric'),
                                                  sid = vector('character')),
             overwrite = TRUE)
#iv tables
dbWriteTable(mydb, "tss_iv", value = data.frame(dt = vector('numeric'),
                                                iv_00530 = vector('numeric'),
                                                sid = vector('character')),
             overwrite = TRUE)
#obs tables
dbWriteTable(mydb, "wtemp_obs", value = data.frame(date = vector('numeric'),
                                                   time = vector('numeric'),
                                                   obs_00011 = vector('numeric'),
                                                   sid = vector('character')),
             overwrite = TRUE)

## double checking
for (i in c('flow_iv', 'flow_dv', 'flow_obs',
            'tss_iv', 'tss_dv', 'tss_obs')){

  print(collect(tbl(mydb, i)))

}

