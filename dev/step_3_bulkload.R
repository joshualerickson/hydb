# bulk load data into hydb
# when you have the data in a format where you can easily bulk then then it's recommended to do that

devtools::document()
# read in the metadata to give an sid to the data
stations <- fetch_hydb('station_metadata')

# we'll enter in the stream temp data from aqs

library(RODBC)
library(tidyverse)

npcw20to24 <- RODBC::odbcConnectAccess2007(r"{C:/Users/joshualerickson/USDA/Northern Region Hydrology - Documents/data-madness/forests/NPCW/wtemp/npcw_temp_2020_2024/NezClearwater_Temp_data_FY20-24.accdb}")

# you'll need to know what specific tables you need/want
# for temperature it's `NRW_AI_Temperature` and for the metadata it's `NRW_AI_TEMPERATURE_SRVY`

# make sure to use `as.is = TRUE` this helps with dttm

temperature <- RODBC::sqlFetch(npcw20to24, 'NRW_AI_Temperature', as.is = TRUE)

temperature_metadata <- RODBC::sqlFetch(npcw20to24, 'NRW_AI_TEMPERATURE_SRVY', as.is = TRUE)

stations_npcw <- stations %>% filter(station_nm  %in% unique(temperature_metadata$NAME))

stations_npcw <- stations_npcw %>% left_join(temperature_metadata %>% select(station_nm = NAME,
                                                                             HDR_FK = HDR_CN))

wtemp_npcw <- temperature %>% left_join(stations_npcw %>% select(sid, HDR_FK)) %>% select(-HDR_FK,
                                                                                     dt = MEAS_DATE,
                                                                                     iv_00011 = TEMPERATURE_STD) %>%
              mutate(iv_00011 = ifelse(iv_00011 < 0 , 0, iv_00011))

wtemp_npcw <- wtemp_npcw %>% dplyr::mutate(dplyr::across(c(2), ~.x*1.8 + 32)) %>% filter(!is.na(sid)) %>% mutate(dt = as_datetime(dt))

wtemp_npcw %>% tibble()
  ggplot(aes(dt, iv_00011, group = sid)) +
  geom_line() +
  facet_wrap(~sid, scales = 'free_x')

# for appending
mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'wtemp_iv', wtemp_npcw)


# now create dailies from the data

wtemp_npcw_dv <- hydb_daily_transform(wtemp_npcw) %>% filter(statistic_type_code != '00006')

DBI::dbAppendTable(mydb, 'wtemp_dv', wtemp_npcw_dv)

hydb_disconnect()
