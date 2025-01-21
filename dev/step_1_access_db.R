# Working with access databases

# sometimes files will come in access databases

# here we'll read in the data and then write to csv's by station_name

# intial metadata


library(RODBC)
library(tidyverse)

npcw20to24 <- RODBC::odbcConnectAccess2007(r"{C:/Users/joshualerickson/USDA/Northern Region Hydrology - Documents/data-madness/forests/NPCW/wtemp/npcw_temp_2020_2024/NezClearwater_Temp_data_FY20-24.accdb}")

# view tables

RODBC::sqlTables(npcw20to24)

# you'll need to know what specific tables you need/want
# for temperature it's `NRW_AI_Temperature` and for the metadata it's `NRW_AI_TEMPERATURE_SRVY`

# make sure to use `as.is = TRUE` this helps with dttm

temperature <- RODBC::sqlFetch(npcw20to24, 'NRW_AI_Temperature', as.is = TRUE)

temperature_metadata <- RODBC::sqlFetch(npcw20to24, 'NRW_AI_TEMPERATURE_SRVY', as.is = TRUE)



