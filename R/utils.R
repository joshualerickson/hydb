#' @title Error Catch paramCd names
#' @param data A user selected data.frame with selected columns.
#' @param table_type vector of parameter_cd
#' @return A logical vector.
#' @noRd
#'
error_catching_param_names <- function(data, table_type){

### these switches could use some help from tolower....

 change_col_names <-

   switch(gsub('_.*', '', table_type),

    'flow' =
      tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "discharge" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "Flow" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "flow" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "flow_" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "flow_00060" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "q" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    stringr::str_detect(.x, 'discharge') ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "Q" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == "discharge_cfs" ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    .x == 'cfs' ~ paste0(sub('.*\\_', '', table_type), "_00060"),
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('iv') ~ 'dt',
    stringr::str_detect(.x, 'remarks|comments|notes') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'comments',
    stringr::str_detect(.x, 'type|statistic_code') & gsub("^[^_]*_", '', table_type) %in% c('dv') ~ 'statistic_type_code',
    TRUE ~ .x))},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

    'tss' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "tss" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "tss_00530" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "tss_" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "total_suspended_sediment" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == 'tss_mg_l' ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == "sediment" ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    .x == 'sed' ~ paste0(sub('.*\\_', '', table_type), "_00530"),
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('iv') ~ 'dt',
    TRUE ~ .x))},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

    'precip' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "precip" ~ paste0(sub('.*\\_', '', table_type), "_00045"),
    .x == 'ppt' ~ paste0(sub('.*\\_', '', table_type), "_00045"),
    .x == "precipitation" ~ paste0(sub('.*\\_', '', table_type), "_00045"),
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('iv') ~ 'dt',
    TRUE ~ .x))},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

    'stage' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
    .x == "GH" ~ paste0(sub('.*\\_', '', table_type), "_00065"),
    .x == "gh" ~ paste0(sub('.*\\_', '', table_type), "_00065"),
    .x == "stage" ~ paste0(sub('.*\\_', '', table_type), "_00065"),
    .x == "stage_height" ~ paste0(sub('.*\\_', '', table_type), "_00065"),
    grepl("gage", .x) ~ paste0(sub('.*\\_', '', table_type), "_00065"),
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
    stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('iv') ~ 'dt',
    .x == 'gage' ~ paste0(sub('.*\\_', '', table_type), "_00065"),
    TRUE ~ .x)
  )},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'airtemp' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  grepl("air_temperature", .x) ~ paste0(sub('.*\\_', '', table_type), "_00021"),
  grepl("air_temp", .x) ~ paste0(sub('.*\\_', '', table_type), "_00021"),
  stringr::str_detect(.x, 'temp') ~ paste0(sub('.*\\_', '', table_type), "_00021"),
  .x == 'airtemp' ~ paste0(sub('.*\\_', '', table_type), "_00021"),
  .x == 'airtemp_' ~ paste0(sub('.*\\_', '', table_type), "_00021"),
  .x == 'airtemp_00021' ~ paste0(sub('.*\\_', '', table_type), "_00021"),
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('iv') ~ 'dt',
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'wtemp' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  .x == 'h2o_temperature_f' ~ paste0(sub('.*\\_', '', table_type), "_00011"),
  stringr::str_detect(.x, 'temp') ~ paste0(sub('.*\\_', '', table_type), "_00011"),
  .x == 'wtemp' ~ paste0(sub('.*\\_', '', table_type), "_00011"),
  .x == 'wtemp_' ~ paste0(sub('.*\\_', '', table_type), "_00011"),
  .x == 'wtemp_00011' ~ paste0(sub('.*\\_', '', table_type), "_00011"),
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('iv') ~ 'dt',
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'svel' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  .x == 'vel' ~ paste0(sub('.*\\_', '', table_type), "_72255"),
  stringr::str_detect(.x, 'velocity') ~ paste0(sub('.*\\_', '', table_type), "_72255"),
  stringr::str_detect(.x, 'fps') ~ paste0(sub('.*\\_', '', table_type), "_72255"),
  .x == 'mean_vel' ~ paste0(sub('.*\\_', '', table_type), "_72255"),
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'swidth' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  stringr::str_detect(.x, 'width') ~ paste0(sub('.*\\_', '', table_type), "_00004"),
  .x == 'stream_width' ~ paste0(sub('.*\\_', '', table_type), "_00004"),
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
}),

'sarea' = tryCatch({data %>% dplyr::rename_with(~dplyr::case_when(
  .x == 'stream_area' ~ paste0(sub('.*\\_', '', table_type), "_82632"),
  stringr::str_detect(.x, 'area') ~ paste0(sub('.*\\_', '', table_type), "_82632"),
  .x == 'mean_area' ~ paste0(sub('.*\\_', '', table_type), "_82632"),
  stringr::str_detect(.x, 'date') & gsub("^[^_]*_", '', table_type) %in% c('dv', 'obs') ~ 'date',
  TRUE ~ .x)
)},
error=function(cond) {
  # Choose a return value in case of error
  return(NA)
})
 )


 if(is.list(change_col_names)){

  col_name_logic <-
     switch(
       sub('.*\\_', '', table_type),
       'dv' = all(names(change_col_names) %in% c(paste0('dv_', param_cd(table_type)), 'date')),
       'iv' = all(names(change_col_names) %in% c(paste0('iv_', param_cd(table_type)), 'dt')),
       'obs' = all(names(change_col_names) %in% c(paste0('obs_', param_cd(table_type)), 'date', 'time'))
     )

 } else {

  col_name_logic <- FALSE

 }


 list(col_name_logic, change_col_names)

}


#' @param table_type A character.
#'
#' @return A character of param code.
#' @noRd
param_cd <- function(table_type) {
  switch(
    gsub('_.*', '', table_type),
    'flow' = '00060',
    'tss' = '00530',
    'precip' = '00045',
    'stage' = '00065',
    'airtemp' = '00021',
    'wtemp' = '00011',
    'swidth' = '00004',
    'svel' = '72255',
    'sarea' = '82632'

  )
}

#' @param stat_type A character.
#'
#' @return A character of stat code.
#' @noRd
stat_cd <- function(stat_type) {
  dplyr::case_when(
    stringr::str_detect(stat_type,'mean') ~ '00003',
    stringr::str_detect(stat_type, 'max') ~ '00001',
    stringr::str_detect(stat_type,'sum') ~ '00006',
    stringr::str_detect(stat_type, 'min') ~ '00002',
    stringr::str_detect(stat_type, 'median') ~ '00008',
    stringr::str_detect(stat_type, 'stdev') ~ '00009',
    stringr::str_detect(stat_type, 'coef_var') ~ '00004'

  )
}


#' 'Clean' a character/factor vector like `janitor::clean_names()` does for data frame columns
#'
#' Most of the internals are from `janitor::clean_names()`
#'
#' @param x a vector of strings or factors
#' @param refactor if `x` is a factor, return a ref-factored factor? Default: `FALSE` == return character vector.
#' @param dupe_count logical.
#'
clean_vec <- function (x, refactor=FALSE, dupe_count = FALSE) {

  require(magrittr, quietly=TRUE)

  if (!(is.character(x) || is.factor(x))) return(x)

  x_is_factor <- is.factor(x)

  old_names <- as.character(x)

  new_names <- old_names %>%
    gsub("'", "", .) %>%
    gsub("\"", "", .) %>%
    gsub("%", "percent", .) %>%
    gsub("^[ ]+", "", .) %>%
    make.names(.) %>%
    gsub("[.]+", "_", .) %>%
    gsub("[_]+", "_", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)

  if(dupe_count){
  dupe_count <- sapply(1:length(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  })

  new_names[dupe_count > 1] <- paste(
    new_names[dupe_count > 1], dupe_count[dupe_count > 1], sep = "_"
  )
  }

  if (x_is_factor && refactor) factor(new_names) else new_names

}


#' Connect to hydb
#'
#' @return Nothing. Side effect connecting to hydb.
#' @export
#'
#' @note Must be a memeber of the `USDA Northern Region Hydrology` sharepoint group.

hydb_connect <- function() {

  windows_path <- normalizePath(file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")), winslash = .Platform$file.sep)

  path <- file.path('/USDA/Northern Region Hydrology - Documents/data-madness/hydb')

  mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(windows_path,path,"/hydb.sqlite"))

  assign('mydb', mydb)

  mydb
}

#' Disconnect to hydb
#'
#' @return Nothing. Side effect connecting to hydb.
#' @export
#'
#' @note Must be a member of the `USDA Northern Region Hydrology` sharepoint group.

hydb_disconnect <- function() {

  DBI::dbDisconnect(mydb)

}
