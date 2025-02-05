#### working with Idaho DEQ

#### read in Idaho sites meta data ####

# this will be dynamic and change (filtering process)

library(tidyverse)
devtools::document()

stations_idaho <- fetch_hydb(table = 'station_metadata',
                             sid %like% '0117%' |
                               sid %like% '0104%'
) %>% dplyr::collect()


# Now read-in the stream layer from IDEQ with au info

stream_305_b <- arcgislayers::arc_select(arcgislayers::arc_open('https://mapcase.deq.idaho.gov/arcgis/rest/services/ID305B_2020_WMS/MapServer/14'))


# now join with metadata

stations_idaho_deq <- stations_idaho %>% left_join(stream_305_b %>% mutate(COMID = as.character(COMID))) %>%
  filter(!is.na(purpose)) %>% sf::st_drop_geometry()


# now write to csv

stations_idaho_deq %>% write_csv(r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\IDEQ\station_metadata_2020_2024.csv}')

