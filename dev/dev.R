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
  meta_data <- read_csv('data/meta_data.csv')
  # get the one you need
  # e.g. meta_data <- meta_data %>% filter(station_nm == 'Pony')

  #then run to get COMID
  comids_nwis <- meta_data %>%
    sf::st_as_sf(coords = c('Long', 'Lat'), crs = 4326) %>%
    split(.$station_nm) %>%
    furrr::future_map(~comids(.)$features$properties$identifier)

  comids_nwis <- tibble(COMID = as.character(comids_nwis),
                        station_nm = names(comids_nwis))

  meta_data <- meta_data %>% left_join(comids_nwis)

  #check and make sure everything is good
  nhdplusTools::get_nldi_basin(list(featureSource = 'comid',
                                    featureID = meta_data[6,]$COMID)) %>%
  mapview::mapview()

#meta data entry
  # meta data sid should follow the pattern;region,forest,district,6-digit #

dbWriteTable(mydb, "station_metadata", meta_data, overwrite = T)

# for deleting mess-ups
#dbExecute(mydb, 'DELETE FROM station_metadata WHERE "station_nm" == "Vermilion Lower"')

# for appending
dbAppendTable(mydb, 'station_metadata', verm)

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
