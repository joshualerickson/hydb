#' @title Error Catch paramCd names
#' @param data A user selected data.frame with selected columns.
#' @param x vector of parameter_cd
#' @return A logical vector.
#' @noRd
#'
error_catching_param_names <- function(data, table_type){



 change_col_names <-

   switch(gsub('_.*', '', table_type),

    'flow' =
      tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "discharge" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "Flow" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "flow" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "q" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "Q" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "discharge_cfs" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == 'cfs' ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "dates" ~ "date",
    .x == "date_time" ~ "dt",
    TRUE ~ .x))},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

    'tss' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "tss" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "total_suspended_sediment" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == 'tss_mg_l' ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "sediment" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == 'sed' ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "dates" ~ "date",
    .x == "date_time" ~ "dt",
    TRUE ~ .x))},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

    'precip' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "precip" ~ paste0(sub('.*\\_', '', table_type), "_00045"),
    .x == 'ppt' ~ paste0(sub('.*\\_', '', table_type), "_00045"),
    .x == "precipitation" ~ paste0(sub('.*\\_', '', table_type), "_00045"),
    .x == "dates" ~ "date",
    .x == "date_time" ~ "dt",
    TRUE ~ .x))},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

    'stage' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "GH" ~ paste0(sub('.*\\_', '', table_type), "00065"),
    .x == "gh" ~ paste0(sub('.*\\_', '', table_type), "00065"),
    .x == "dates" ~ "date",
    .x == "date_time" ~ "dt",
    .x == 'gage' ~ paste0(sub('.*\\_', '', table_type), "00065"),
    TRUE ~ .x)
  )},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'airtemp' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  .x == grepl("air_temperature", x) ~ paste0(sub('.*\\_', '', table_type), "00021"),
  .x == grepl("air_temp", x) ~ paste0(sub('.*\\_', '', table_type), "00021"),
  .x == "dates" ~ "date",
  .x == "date_time" ~ "dt",
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'wtemp' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  .x == grepl("h2o_temperature", .x) ~ paste0(sub('.*\\_', '', table_type), "00011"),
  .x == grepl("h2o_temp", .x) ~ paste0(sub('.*\\_', '', table_type), "00011"),
  .x == "dates" ~ "date",
  .x == "date_time" ~ "dt",
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
})
 )

 param_cd <- switch(
   gsub('_.*', '', table_type),
   'flow' = '00060',
   'tss' = '00530',
   'precip' = '00045',
   'stage' = '00065',
   'airtemp' = '00021',
   'wtemp' = '00011'

 )

 if(is.list(change_col_names)){

  col_name_logic <-
     switch(
       sub('.*\\_', '', table_type),
       'dv' = all(names(change_col_names) %in% c(paste0('dv_', param_cd), 'date')),
       'iv' = all(names(change_col_names) %in% c(paste0('iv_', param_cd), 'dt')),
       'obs' = all(names(change_col_names) %in% c(paste0('obs_', param_cd), 'date', 'time'))
     )

 } else {

  col_name_logic <- FALSE

 }


 list(col_name_logic, change_col_names)

}



#' @title Error Catching Table Input
#' @param data data.frame from user input
#' @param table_type Character from selected table.
#' @noRd
#' @return A logical.
error_catching_input <- function(data, table_type) {

    switch(
      sub('.*\\_', '', table_type),
      'dv' = length(data) == 2,
      'iv' = length(data) == 2,
      'obs' = length(data) %in% c(2,3),
    )

}
