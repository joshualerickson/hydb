library(DBI)
library(RSQLite)
library(tidyverse)
library(sf)
devtools::document()

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

# now give each station its sid
# read in the admin districts
admin_districts <- read_sf('Z:/simple_features/lands/admin_units_district.shp')%>%
  st_transform(4326) %>% st_make_valid()

#### Get COMID with meta data long, lat

### Troy
meta_data <- tibble(
  station_nm = 'Basin Creek',
  Long = -115.482270,
  Lat = 48.930529

)

### Libby

meta_data <- tribble(
  ~station_nm, ~Lat, ~Long,
  'Bobtail Creek', 	48.42768,	-115.60374,
  'Cowell Creek',	48.18398,	-115.41273,
  'Cripple Horse',	48.47699,	-115.25076,
  'Flattail Creek', 	48.62419,	-115.71501,
  'Flower Creek',	48.34466,	-115.60665,
  'Himes Creek', 	47.95416,	-115.33779,
  'Pipe Creek', 	48.42781,	-115.59556,
  'Quartz Creek',	48.44571,	-115.63048,
  'Silver Butte',	48.00751,	-115.36769,
  'West Fisher',	48.05183,	-115.42873,
  'West Fork Quartz', 	48.49991,	-115.67749,
  'Wolf Creek at Fairview',	48.35408,	-115.03883
)

### cabinet

#cabinet_sf <- mapedit::editAttributes(cabinet_sf)
#write_sf(cabinet_sf, 'dev/sites.gpkg', layer = 'cabinet')
comids_nwis <- cabinet_sf %>%
  split(.$id) %>%
  furrr::future_map(~comids(.)$features$properties$identifier)

# get the one you need
# e.g. meta_data <- meta_data %>% filter(station_nm == 'Pony')

#then run to get COMID
comids_nwis <- meta_data %>%
  sf::st_as_sf(coords = c('Long', 'Lat'), crs = 4326) %>%
  split(.$station_nm) %>%
  furrr::future_map(~comids(.)$features$properties$identifier)

comids_nwis <- tibble(COMID = as.character(comids_nwis),
                      station_nm = names(comids_nwis))

meta_data <- cabinet_sf %>%
  st_drop_geometry() %>%
  rename(station_nm = 'id') %>% select(-comments) %>%
  left_join(comids_nwis)

#check and make sure everything is good
# nhdplusTools::get_nldi_basin(list(featureSource = 'comid',
#                                   featureID = meta_data[1,]$COMID)) %>%
#   mapview::mapview()

#check and make sure everything is good
basins = list()
for(i in meta_data$COMID){
  blist <- nhdplusTools::get_nldi_basin(list(featureSource = 'comid',
                                             featureID = i))

  basins <- append(basins, list(blist))
}

basins_sf <- bind_rows(basins)

st_write(basins_sf, 'dev/sites.gpkg', layer = 'cabinet_polygons')

mapview::mapview(basins_sf) + st_as_sf(meta_data, coords = c('Long', 'Lat'), crs = 4326) %>%
  mapview::mapview(zcol = 'station_nm')

#meta data entry
# meta data sid should follow the pattern;region,forest,district,6-digit #


meta_data_sf <- meta_data %>%
                mutate(Long = st_coordinates(cabinet_sf)[,1],
                       Lat = st_coordinates(cabinet_sf)[,2]) %>%
                sf::st_as_sf(coords = c('Long', 'Lat'), crs = 4326) %>%
                st_intersection(admin_districts %>% select(DISTRICTOR, FORESTNAME, DISTRICTNA)) %>% mutate(sid = DISTRICTOR)

site_off_fs_land <- meta_data[!meta_data$station_nm %in% meta_data_sf$station_nm,]
site_off_fs_land$sid <- '011405'
site_off_fs_land$forest = 'Kootenai National Forest'
site_off_fs_land$district = 'Libby Ranger District'
site_off_fs_land$region = 'Northern Region'
site_off_fs_land$Long = st_coordinates(cabinet_sf[!cabinet_sf$id %in% meta_data_sf$station_nm,])[,1]
site_off_fs_land$Lat = st_coordinates(cabinet_sf[!cabinet_sf$id %in% meta_data_sf$station_nm,])[,2]


meta_data <- meta_data_sf %>% bind_cols(tibble(Long = st_coordinates(.)[,1],Lat = st_coordinates(.)[,2])) %>% st_drop_geometry() %>%
  select(station_nm,
                                     forest = FORESTNAME,
                                     district = DISTRICTNA,
                                     Long, Lat, sid) %>% bind_cols(tibble(region = "Northern Region",
                                                                          COMID = meta_data[meta_data$station_nm %in% meta_data_sf$station_nm,]$COMID)) %>%
  bind_rows(site_off_fs_land)



#now add unique 5-digit code
add_to_sid <- as.character(c(paste0('0000', seq(2,9,1)), paste0('000', seq(10,12,1))))

meta_data <- meta_data %>% arrange(desc(Lat)) %>%
  mutate(sid = paste0(sid, add_to_sid))

# add to db

dbWriteTable(mydb, "station_metadata", meta_data, overwrite = T)

# for deleting mess-ups
#dbExecute(mydb, 'DELETE FROM station_metadata WHERE "station_nm" == "Vermilion Lower"')

# for appending
DBI::dbAppendTable(hydb_connect(), 'station_metadata', meta_data)

+#### for creating new tables

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
