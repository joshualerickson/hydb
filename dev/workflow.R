### workflow
devtools::document()

library(tidyverse)

site_data <- fetch_hydb('station_metadata')

site_data <- md[md$forest == 'Kootenai National Forest',]


flow_kootenai_iv <- fetch_hydb(table = 'flow_iv')
min(flow_kootenai_iv$dt)

flow_kootenai_iv %>% group_by(sid) %>% summarise(min_date = min(dt),
                                                 max_date = max(dt),
                                                 count = n()) %>% left_join(site_data, by = 'sid') %>% view()

flow_kootenai_flow_obs <- fetch_hydb(table = 'flow_obs',
                                     sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                       site_data$district == 'Cabinet Ranger District',]$sid)

libby_flow_obs <- fetch_hydb(table = 'flow_obs',
                                     sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                       site_data$district == 'Libby Ranger District',]$sid)

libby_flow_dv <- fetch_hydb(table = 'flow_dv',
                                     sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                       site_data$district == 'Libby Ranger District',]$sid)

hlc_flow_dv <- fetch_hydb(table = 'flow_dv',
                                     sid = site_data[site_data$forest == 'Helena-Lewis and Clark National Forest',]$sid)
hlc_flow_obs <- fetch_hydb(table = 'flow_obs',
                                     sid = site_data[site_data$forest == 'Helena-Lewis and Clark National Forest',]$sid)

fetch_hydb('flow_iv', sid = '01140500012') %>% view()

libby_flow_dv %>% left_join(site_data) %>%
  ggplot(aes(date, dv_00060)) +
  geom_line() +
  geom_point(data = libby_flow_obs%>% left_join(site_data),
             aes(date, obs_00060)) +
  facet_wrap(~station_nm, scales = 'free')

hlc_flow_dv %>% filter(statistic_type_code == '00003') %>%
  left_join(site_data) %>%
  ggplot(aes(date, dv_00060)) +
  geom_line() +
  facet_wrap(~station_nm, scales  = 'free')


cabinet_iv_files <- list.files(r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\KNF\flow\iv\Cabinet}')

cabinet_iv_data <- list.files(paste0(r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\KNF\flow\iv\Cabinet\}', cabinet_iv_files[[1]]))

path <- paste0(r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\KNF\flow\iv\Cabinet\}', cabinet_iv_files[[1]])

cabinet_iv_data <- map(paste0(path,'\\', cabinet_iv_data), readxl::read_xlsx)

cab_iv_flow <- fetch_hydb(table = 'flow_iv', sid = '01140790001')
cab_iv_flow2 <- fetch_hydb(table = 'flow_iv', sid = '01140700012')

cab_iv_flow %>%
  ggplot(aes(dt, iv_00060)) +
  geom_line() +
  facet_wrap(~sid, scales = 'free')

libby_stage_obs <- fetch_hydb(table = 'stage_obs',
                                     sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                       site_data$district == 'Libby Ranger District',]$sid)



libby_flow_obs <- fetch_hydb(table = 'flow_obs',
                              sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                site_data$district == 'Libby Ranger District',]$sid)



libby_width_obs <- fetch_hydb(table = 'swidth_obs',
                              sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                site_data$district == 'Libby Ranger District',]$sid)

libby_area_obs <- fetch_hydb(table = 'sarea_obs',
                              sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                site_data$district == 'Libby Ranger District',]$sid)

libby_tss_obs <- fetch_hydb(table = 'tss_obs',
                              sid = site_data[site_data$forest == 'Kootenai National Forest' &
                                                site_data$district == 'Libby Ranger District',]$sid)

libby_tss_obs %>% left_join(site_data, by = 'sid') %>%
  left_join(libby_flow_obs %>% left_join(site_data %>% select(sid, station_nm), by = 'sid'), by = c('sid', 'date', 'station_nm')) %>%
  ggplot(aes(obs_00530, obs_00060)) +
  geom_point(aes(color = year(date))) +
  # scale_y_continuous(trans = 'log') +
  # scale_x_continuous(trans = 'log') +
  facet_wrap(~station_nm, scales = 'free')


cabinet_flow_obs <- flow_kootenai_flow_obs %>% left_join(site_data, by = c('sid'))



cabinet_flow_obs %>% view()



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


test <- libby_tss_obs %>% left_join(site_data, by = 'sid') %>%
  left_join(libby_flow_obs %>% left_join(site_data %>% select(sid, station_nm), by = 'sid'), by = c('sid', 'date', 'station_nm'))

GWalkR::gwalkr(test)

path <- r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\KNF\flow\iv\Cabinet\2017_Finals\EF_Bull_WY_Data_2017_30_min_final.xlsx}'

ef1 <- readxl::read_xlsx(path = path) %>% janitor::clean_names() %>% mutate(name = 'ef1',
                                                                            id = row_number(),
                                                                            date = as_date(date))
ef2 <- readxl::read_xlsx(path = path, sheet = 2) %>% janitor::clean_names() %>% mutate(name = 'ef2',
                                                                                       id = row_number(),
                                                                                       date = as_date(date))

ef <- bind_rows(ef1 %>% select(-time), ef2 %>% select(-time))

ef_final <- ef %>% group_by(name, date) %>% summarise(across(discharge_cfs:air_temp_f, ~mean(.x, na.rm = T))) %>% ungroup()

ef_final <- ef_final %>% group_by(date) %>% summarise(discharge_cfs = sum(discharge_cfs),
                                                      water_temp_f = mean(degrees_f, na.rm = T),
                                                      air_temp_f = mean(air_temp_f, na.rm = T))
write_csv(ef_final,  r'{C:\Users\joshualerickson\USDA\Northern Region Hydrology - Documents\data-madness\forests\KNF\flow\iv\Cabinet\2017_Finals\EF_Bull_WY_Data_2017_dv_final.csv}')

devtools::document()
cabinet_iv_data <- fetch_hydb('airtemp_iv', tbl_only = TRUE)

site_data <- fetch_hydb('station_metadata', tbl_only = TRUE)

cabinet_iv_data <- cabinet_iv_data %>%
  left_join(site_data, copy = TRUE) %>%
  filter(district  %in%  c('Cabinet Ranger District', 'Plains/Thompson Falls Ranger District')) %>%
  collect()

hydb_disconnect()

cab_dv_000060 <- cabinet_iv_data %>%
  mutate(date = date(as_datetime(dt))) %>%
  group_by(sid, date) %>%
  summarise(across(dplyr::any_of('iv_00021'),
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

cab_dv_000060 <- cab_dv_000060 %>% mutate(statistic_type_code = stat_cd(name)) %>%
  select(-name) %>%
  rename(dv_00021 = 'value')

mydb <- hydb_connect()

DBI::dbAppendTable(mydb, 'airtemp_dv', cab_dv_000060)

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
devtools::document()

library(tidyverse)

site_data <- fetch_hydb('station_metadata')

verm_sid <- site_data %>% filter(str_detect(station_nm, 'Red Bridge'))

verm <- fetch_hydb('flow_dv', sid = verm_sid$sid) %>% filter(statistic_type_code == '00003')

verm <- verm %>% Riverwave::prep_flow(dv_00060)

verm_wy <- verm %>% group_by(wy) %>% slice_max(dv_00060) %>% filter(dv_00060 > 100)

library(wildlandhydRo)
library(smwrBase)
library(evd)

library(Riverwave)
um <- readRDS('Z:/GIT/rw_henry/data/um.rds')

rw_hydrograph(verm, value_name = dv_00060, wy_month = 10)

rw_rastergraph(verm, value_name = dv_00060,q1 = 800, q2 = 1250, wy_month = 10)

rw_percentiles_plot(verm, value_name = dv_00060,q1 = 800, q2 = 1250, q5 = 2113,wy_month = 10)

rw_3d(data = verm %>% mutate(dv_00060 = dv_00060*0.028316831998814504),
      value_name = dv_00060,
      q1 = 800*0.028316831998814504,
      q2 = 1250*0.028316831998814504, userMatrix = um)

rgl::par3d()

verm_dist <- batch_distribution(verm_wy, dv_00060)

plot_densDist(verm_dist[['lpearson']])

verm_rep <- reportDist(verm_dist)
plot_reportDist(verm_rep)

plot(verm_wy$date, verm_wy$dv_00060)
