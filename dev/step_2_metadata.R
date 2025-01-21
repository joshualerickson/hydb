# metadata setup
# this example will build on information from step 1
# essentially joining spatial with the data. This isn't always the case and most of the time metadata will be added manually via csv.
# either way (how it gets here) you'll need an sf point object to finish before appending to hydb.

devtools::document()
library(tidyverse)
library(sf)

aqs_npcw <- read_sf(r"{T:/FS/NFS/R01/Program/2500WatershedAirMgmt/GIS/WorkSpace/jerickson/Spatial/scratch/npcw_temp_site.shp}")

aqs_npcw_2020_2024 <- aqs_npcw %>% filter(SURVEY_CN  %in% unique(temperature_metadata$HDR_CN))

aqs_npcw_2020_2024_sf_point <- st_centroid(aqs_npcw_2020_2024)

# clean up data frame to follow metadata schema

aqs_npcw_2020_2024_sf_point <- aqs_npcw_2020_2024_sf_point %>%
                               mutate(comments = paste0('Water ', SURVEY_TYP,'; SURVEY NAME: ',SURVEY_NAM ))%>%
                               select(station_nm = SURVEY_LOC,
                                      purpose = PROTOCOL_N,
                                      comments
                                      )
# check overlapping sites
# this checks for duplicates and then collapse with character columns
aqs_npcw_2020_2024_sf_point_clean <- hydb_clean_duplicate_metadata(aqs_npcw_2020_2024_sf_point)

# now run metadata function

aqs_npcw_2020_2024_df <- hydb_setup_metadata(aqs_npcw_2020_2024_sf_point_clean)

# for appending
mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'station_metadata', aqs_npcw_2020_2024_df)
