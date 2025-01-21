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
    .x == "tss" ~ paste0(sub('.*\\_', '', table_type), "_70288"),
    .x == "tss_" ~ paste0(sub('.*\\_', '', table_type), "_70288"),
    .x == "total_suspended_sediment" ~ paste0(sub('.*\\_', '', table_type), "_70288"),
    .x == 'tss_mg_l' ~ paste0(sub('.*\\_', '', table_type), "_70288"),
    .x == "sediment" ~ paste0(sub('.*\\_', '', table_type), "_70288"),
    .x == 'sed' ~ paste0(sub('.*\\_', '', table_type), "_70288"),
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
    'tss' = '70288',
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

#' @param stat_type A character.
#'
#' @return A character of stat code.
#' @noRd
stat_cd_to_name <- function(stat_type) {
  dplyr::case_when(
    stringr::str_detect(stat_type,'00003') ~ 'mean',
    stringr::str_detect(stat_type, '00001') ~ 'max',
    stringr::str_detect(stat_type,'00006') ~ 'sum',
    stringr::str_detect(stat_type, '00002') ~ 'min',
    stringr::str_detect(stat_type, '00008') ~ 'median',
    stringr::str_detect(stat_type, '00009') ~ 'stdev',
    stringr::str_detect(stat_type, '00004') ~ 'coef_var'

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
#' @param path A character vector file path to `.sqlite` database.
#' @return Nothing. Side effect connecting to hydb.
#' @export
#'
#' @note Must be a member of the `USDA Northern Region Hydrology` sharepoint group if `path = NULL`.

hydb_connect <- function(path = NULL) {

  if(is.null(path)){
  windows_path <- normalizePath(file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")), winslash = .Platform$file.sep)

  path <- file.path('/USDA/Northern Region Hydrology - Documents/data-madness/hydb')

  mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(windows_path,path,"/hydb.sqlite"))

  } else {

  mydb <- DBI::dbConnect(RSQLite::SQLite(), path)

  }

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


#' comids
#'
#' @description
#' Get NHDPLus comid by point location.
#'
#' @param point An sf POINT object
#'
#' @return A character vector with comid
#' @export
#'
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
#' Clean Spatial Duplicates
#'
#' @description
#' Get cleaned duplicates.
#'
#' @param point An sf POINT object
#'
#' @return An sf object
#' @export
#'
hydb_clean_duplicate_metadata <- function(point) {

  eq_list <- sf::st_equals(point)

  group_mapping <- sapply(seq_along(eq_list), function(i) {min(unlist(eq_list[[i]]))})

  point <- point %>%
    dplyr::mutate(group_id = group_mapping) %>%
    dplyr::group_by(group_id) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.character), ~paste(unique(.), collapse = ", "), .names = "{col}"),
              across(dplyr::where(is.numeric), first),
              geometry = sf::st_union(geometry)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-group_id)
}

#' Setup Metadata
#'
#' @param point An sf POINT cleaned.
#'
#' @return A `tibble()` ready for appending to hydb.
#' @export
#'
hydb_setup_metadata <- function(point) {

  if(any(!names(point) %in% c('station_nm', 'purpose', 'comments', 'geometry'))) stop(message('need corrent column names'))

  # now give each station its sid
  # read in the admin districts

  admin_districts <- read_sf('Z:/simple_features/lands/admin_units_district.shp')  %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid() %>%
    dplyr::select(sid = DISTRICTOR,
                  forest = FORESTNAME,
                  district = DISTRICTNA)

  meta_data <- point %>%
    st_transform(4326) %>%
    st_make_valid()

  meta_data_copy <- meta_data %>%
    st_intersection(admin_districts)

  if(any(!meta_data$station_nm %in% meta_data_copy$station_nm)){
    site_off_fs_land <- meta_data[!meta_data$station_nm %in% meta_data_copy$station_nm,] %>%
      dplyr::mutate(
        sid = admin_districts[sf::st_nearest_feature(., admin_districts),]$sid,
        forest = admin_districts[sf::st_nearest_feature(., admin_districts),]$forest,
        district = admin_districts[sf::st_nearest_feature(., admin_districts),]$district,
        comments = paste0('Off NFS Land; ',comments)
      )

    meta_data_copy <- meta_data_copy %>% dplyr::bind_rows(site_off_fs_land) %>% sf::st_as_sf()

  }


  meta_data_coordinates <- meta_data_copy %>% bind_cols(tibble(Long = st_coordinates(.)[,1],Lat = st_coordinates(.)[,2])) %>% st_drop_geometry()
  meta_data_copy <- meta_data_copy %>% bind_cols(meta_data_coordinates %>% select(Long, Lat))

  #check to see if it already exists and also get a unique suffix

  md <- fetch_hydb('station_metadata', tbl_only = T)

  forests_filter <- unique(meta_data_copy$forest)

  md <- md %>% dplyr::filter(forest %in% forests_filter) %>% dplyr::collect()

  md_start <- max(as.numeric(substr(md$sid, 7, nchar(md$sid))))


  #now add unique 5-digit code

  meta_data_copy <- meta_data_copy %>%
    arrange(desc(Lat)) %>%
    mutate(sid = paste0(sid, sprintf("%05d", md_start + row_number())))

  #then run to get COMID
  comids_nwis <- meta_data_copy %>%
    split(.$sid) %>%
    purrr::map(~comids(.)$features$properties$identifier)

  comids_nwis <- tibble(COMID = as.character(comids_nwis),
                        sid = names(comids_nwis))

  meta_data_final <- meta_data_copy  %>% left_join(comids_nwis) %>% sf::st_drop_geometry()


}


#' Transform IV to DV
#'
#' @param data A data.frame
#'
#' @return A `tibble()`.
#' @export
#' @note Need to have columns `dt`, `sid` and `iv_*` and `iv_*` needs to be column position 2.

hydb_daily_transform <- function(data) {

  data_add_daily_stats <-  data %>%
    dplyr::mutate(date = lubridate::date(dt)) %>%
    dplyr::group_by(sid, date) %>%
    dplyr::summarise(dplyr::across(dplyr::any_of(dplyr::starts_with('iv_')),
                     list(
                       sum = ~sum(.x, na.rm = TRUE),
                       max = ~max(.x, na.rm = TRUE),
                       min = ~min(.x, na.rm = TRUE),
                       mean = ~mean(.x, na.rm = TRUE),
                       median = ~median(.x, na.rm = TRUE),
                       stdev = ~sd(.x, na.rm = TRUE),
                       coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE))))  %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(dplyr::starts_with('iv_'))%>%
    dplyr::mutate(statistic_type_code = stat_cd(name))

  param_type <- paste0('dv',unique(substr(data_add_daily_stats$name, 3, 8)))

  variable_to_rename <- 'value'

  data_add_daily_stats %>%
  dplyr::rename(!!param_type := !!rlang::sym('value')) %>%
  dplyr::select(-name)

}

