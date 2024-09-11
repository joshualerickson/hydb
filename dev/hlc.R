library(tidyverse)
library(sf)
devtools::document()

watershed_data <- arrow::read_feather('Z:/GIT/accesshydro/data/hlc/watershed_data.feather') %>% janitor::clean_names() %>%
                  tibble() %>%
                  rename(station_nm = 'site_id')

watershed_data <- watershed_data %>%
  mutate(time =  strftime(time, format="%H:%M:%S"),
         date = as.Date(date))


watershed_data <- watershed_data %>% filter(!is.na(date))

length(unique(watershed_data$station_nm))

site_data_hlc <- fetch_hydb('station_metadata') %>% filter(forest == 'Helena-Lewis and Clark National Forest')

length(unique(watershed_data$station_nm))

water_tss <- watershed_data %>%
             select(date, time, station_nm, dv_00530 = 'tss_mg_l',comments, tss_method) %>%
             filter(!is.na(dv_00530)) %>%
             mutate(dt = as_datetime(paste0(date,' ', time))) %>%
             group_by(station_nm, date) %>%
             add_count() %>%
             ungroup() %>%
             left_join(site_data_hlc %>% select(sid, station_nm))%>%
             select(-station_nm) %>%
             filter(!is.na(sid)) %>%
            filter(!is.na(dv_00530)) %>%
            mutate(tss_method = ifelse(tss_method == '?' & is.na(time), 'ISCO', tss_method),
                   obs_00530 = ifelse(tss_method != 'ISCO', dv_00530, NA_real_),
                   dv_00530 = ifelse(tss_method == 'ISCO', dv_00530, NA_real_))

water_temp_f %>% filter(str_detect(sid, "^[000]"))

mydb <- hydb_connect()

hlc_obs_00060 <- water_tss%>%
  filter(!is.na(dv_00530)) %>%
  left_join(site_data_hlc %>% select(sid, station_nm))  %>%
  select(date, sid,dv_00530)


DBI::dbAppendTable(mydb, 'tss_dv', hlc_obs_00060)

DBI::dbDisconnect(mydb)

write_csv(obs_00060, r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\HLC\flow\obs\hlc_obs_06000_accessdb_2017_jle.csv}')

hlc_dv_00060 <- water_q %>% filter(!is.na(iv_00060)) %>% dplyr::select(dt, iv_00060, sid) %>%
             mutate(date = date(dt)) %>%
             group_by(sid, date) %>%
  summarise(across(dplyr::any_of('iv_00060'),
                   list(
                     sum = ~sum(.x, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     mean = ~mean(.x, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     stdev = ~sd(.x, na.rm = TRUE),
                     coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE))))  %>%
  ungroup() %>%
  pivot_longer(starts_with('iv_'))

hlc_dv_00060 <- hlc_dv_00060 %>% mutate(statistic_type_code = stat_cd(name)) %>% select(-name) %>% rename(dv_00060 = 'value')

mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'flow_dv', hlc_dv_00060)

DBI::dbDisconnect(mydb)


DBI::dbWriteTable(mydb,'stat_codes',tribble(~statistic_type_code,	~statistic_type_name,	~statistic_type_description,
'00001',	'MAXIMUM',	'MAXIMUM VALUES',
'00002',	'MINIMUM',	'MINIMUM VALUES',
'00003',	'MEAN',	'MEAN VALUES',
'00004', 'CV', 'COEFFICIENT OF VARIATION',
'00006',	'SUM',	'SUMMATION VALUES',
'00008',	'MEDIAN',	'MEDIAN VALUES',
'00009',	'STD',	'STANDARD DEVIATION VALUES'))



DBI::dbAppendTable(mydb,'param_codes',
                  tribble(~parameter_type_code, ~group_name, ~parameter_type_description,
        '82632',"Information", "Area, cross section, square feet"
))



hlc_flow_obs %>% view()


libby_dv <- fetch_hydb('flow_dv', sid = '01140500002')
print.htmlwidget <- function(widget){
  temp_file <- paste(tempfile('widget'), 'html', sep = '.')
  htmlwidgets::saveWidget(widget, temp_file, selfcontained = FALSE)
  shell(sprintf("start chrome -app=file://%s", temp_file))
}
libby_dv %>% GWalkR::gwalkr()

glimpse(watershed_data)

watershed_data %>% filter(!is.na(watertemp_c))



hlc_dv_000060 <- hlc_flow_iv %>%
  mutate(date = date(dt)) %>%
  group_by(sid, date) %>%
  summarise(across(dplyr::any_of('iv_00060'),
                   list(
                     sum = ~sum(.x, na.rm = TRUE),
                     max = ~max(.x, na.rm = TRUE),
                     min = ~min(.x, na.rm = TRUE),
                     mean = ~mean(.x, na.rm = TRUE),
                     median = ~median(.x, na.rm = TRUE),
                     stdev = ~sd(.x, na.rm = TRUE),
                     coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE))))  %>%
  ungroup() %>%
  pivot_longer(starts_with('iv_'))

hlc_dv_000060 <- hlc_dv_000060 %>% mutate(statistic_type_code = stat_cd(name)) %>% select(-name) %>% rename(dv_00060 = 'value')

mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'flow_dv', hlc_dv_000060)

DBI::dbDisconnect(mydb)


fruit <- c("apple", "banana", "pear", "pineapple")
str_detect(fruit, "a")
str_detect(fruit, "^a")
str_detect(fruit, "a$")
str_detect(fruit, "b")
str_detect(fruit, "[aeiou]")

# Also vectorised over pattern
str_detect("aecfg", letters)

# Returns TRUE if the pattern do NOT match
str_detect(fruit, "^pea")
