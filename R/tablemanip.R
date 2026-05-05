#' Prep 'hydb' Table
#' @describeIn This function preps the data for more analysis.
#' @param data A `hydb` table.
#' @param wy_month Numeric. Month starting water year.
#'
#' @importFrom dplyr "%>%"
#'
#' @return `data` but with added attributes
#' @export
#'
hydb_prep_data <- function(data, wy_month = 10) {

  leap_years <- c(seq(1832,by = 4, length.out = 2000))


  data <- data %>%
    dt_to_tibble()

  colname1 <- colnames(data)[!is.na(match(colnames(data), c('dt', 'date')))]

  data %>%
    dplyr::mutate(year = lubridate::year(.data[[colname1]]),
           month = lubridate::month(.data[[colname1]]),
           day = lubridate::day(.data[[colname1]]),
           doy=lubridate::yday(.data[[colname1]]),
           wy_doy = ifelse(!(year %in% leap_years),ifelse(doy >= month_to_doy(wy_month, leap = F),
                                                          doy-month_to_doy(wy_month, leap = F)+1,
                                                          (365-month_to_doy(wy_month, leap = F)+1+doy)),
                           ifelse(doy >= month_to_doy(wy_month, leap = T),
                                  doy-month_to_doy(wy_month, leap = T)+1,
                                  (366-month_to_doy(wy_month, leap = T)+1+doy))),
           month_day = stringr::str_c(month, day, sep = "-"),
           wy = waterYear(.data[[colname1]], wy_month, TRUE),
           month_abb = factor(month.abb[month], levels = month.abb),
           month_day = stringr::str_c(month, day, sep = "-")) %>%
    add_date_counts()

}

#' Setup Metadata
#' @describeIn A function to setup user submitted stations metadata.
#' @param point An sf POINT cleaned.
#' @param column_map A vector of names to change in point sf column names, (NULL) default.
#'
#' @return A `tibble()` ready for appending to `hydb`.
#' @notes In `column_map` argument use a named vector, e.g. `my_column_map <- c(station_nm = "SITE_NAME", district = "DISTRICT", comments = "REMARKS")`.
#' @export
#'
hydb_setup_metadata <- function(point, column_map = NULL) {

  # 0. Initial Input Validation and Setup
  if (!inherits(point, "sf")) {
    stop("Input 'point' must be an sf object.")
  }

  # Define the target hydb station_metadata column names
  # These are the columns the final output will have, and what the function expects internally.
  hydb_target_cols <- c("station_nm", "forest", "district", "Long", "Lat",
                        "sid", "region", "COMID", "comments", "purpose")

  # 1. Apply column mapping and standardize names
  if (!is.null(column_map)) {
    # Check if all user-specified columns in the map actually exist in 'point'
    missing_user_cols_in_map <- setdiff(unname(column_map), names(point))
    if (length(missing_user_cols_in_map) > 0) {
      stop(paste0("The following user columns specified in 'column_map' are not found in the input 'point' data: ",
                  paste(missing_user_cols_in_map, collapse = ", ")))
    }
    # Rename columns using the provided map
    point <- point %>% dplyr::rename(!!!column_map)
    message("Columns renamed based on provided map.")
  }

  # 2. Ensure all `hydb_target_cols` exist, adding NA if missing
  # This makes the data frame conform to the expected internal schema before processing.
  cols_to_add <- setdiff(hydb_target_cols, names(point))
  if (length(cols_to_add) > 0) {
    message(paste("Adding missing hydb columns as NA:", paste(cols_to_add, collapse = ", ")))
    for (col in cols_to_add) {
      # Default to character NA, adjust type (e.g., NA_real_) if you know the column's type
      point[[col]] <- NA_character_
    }
  }

  # Ensure the `sid` column from the input is of character type if it exists
  # before it's used in paste0 later.
  if ("sid" %in% names(point)) {
    point$sid <- as.character(point$sid)
  }

  # --- Start of your original function logic, adapted to standardized names ---

  # Transform to WGS84 and make valid for spatial operations
  meta_data <- point %>%
    sf::st_transform(4326) %>%
    sf::st_make_valid()

  # Perform spatial intersection to assign initial forest/district/sid
  # Ensure only relevant columns from admin_districts are selected to avoid
  # bringing in too many extra columns from the spatial join.
  # Using st_join with sf::st_intersects to ensure one-to-one or one-to-many
  # and then handle the one-to-one selection
  meta_data_intersect <- meta_data %>%
    sf::st_join(admin_districts %>% dplyr::select(forest_admin = forest, district_admin = district, sid_base = sid),
                join = sf::st_intersects,
                left = TRUE) # Keep all points, even if they don't intersect

  # Fill in forest, district, sid from intersection where available
  # Prioritize existing 'district' if it was mapped from user input, otherwise use spatial result
  meta_data_copy <- meta_data_intersect %>%
    dplyr::mutate(
      forest = dplyr::if_else(is.na(forest_admin), forest, forest_admin), # Use admin_districts forest
      district = dplyr::if_else(is.na(district_admin), district, district_admin), # Use admin_districts district
      # The base SID for generation comes from admin_districts
      sid = dplyr::if_else(is.na(sid_base), sid, sid_base)
    ) %>%
    dplyr::select(-forest_admin, -district_admin, -sid_base) # Remove temporary join columns


  # Handle sites that are still off NFS land (i.e., no intersection)
  # These will have NA for 'forest' and 'district' after the st_join if they didn't intersect
  if(any(is.na(meta_data_copy$forest) | is.na(meta_data_copy$district))){
    message("Some stations are off NFS land or not within any known district. Finding nearest administrative district.")
    # Identify points that didn't intersect any admin district
    off_nfs_land_idx <- which(is.na(meta_data_copy$forest) | is.na(meta_data_copy$district))
    site_off_fs_land <- meta_data_copy[off_nfs_land_idx,]

    if (nrow(site_off_fs_land) > 0) {
      # Find nearest admin district for these points
      nearest_idx <- sf::st_nearest_feature(site_off_fs_land, admin_districts)
      nearest_admin_info <- admin_districts[nearest_idx,] %>%
        sf::st_drop_geometry() %>%
        dplyr::select(forest_nearest = forest, district_nearest = district, sid_nearest = sid)

      # Update the off-NFS sites with info from nearest district
      site_off_fs_land <- site_off_fs_land %>%
        dplyr::bind_cols(nearest_admin_info) %>%
        dplyr::mutate(
          forest = dplyr::if_else(is.na(forest), forest_nearest, forest),
          district = dplyr::if_else(is.na(district), district_nearest, district),
          sid = dplyr::if_else(is.na(sid), sid_nearest, sid),
          comments = paste0('Off NFS Land (nearest district assigned); ', dplyr::if_else(is.na(comments), "", comments))
        ) %>%
        dplyr::select(-forest_nearest, -district_nearest, -sid_nearest) # Remove temporary columns

      # Update the main meta_data_copy with the corrected off-NFS sites
      meta_data_copy[off_nfs_land_idx,] <- site_off_fs_land
    }
  }

  # Extract coordinates (Long/Lat) from geometry. This is the definitive source.
  meta_data_copy <- meta_data_copy %>%
    dplyr::mutate(
      Long = sf::st_coordinates(.)[,1],
      Lat = sf::st_coordinates(.)[,2]
    )

  # Check for existing stations and generate unique SIDs
  md_stations <- hydb_fetch('station_metadata', collect = TRUE)

  # Filter existing metadata by districts present in the current batch of points
  district_filter <- unique(meta_data_copy$district)
  md <- md_stations %>% dplyr::filter(district %in% district_filter)

  # Determine the starting suffix for new SIDs based on existing SIDs in these districts
  md_start <- 0
  if (nrow(md) > 0) {
    # Extract numeric part of SID (assuming format like FSTX#####)
    # Handle potential NAs or non-matching formats gracefully
    numeric_sids <- suppressWarnings(as.numeric(substr(md$sid, 7, nchar(md$sid))))
    md_start <- max(numeric_sids, na.rm = TRUE)
    if (!is.finite(md_start)) md_start <- 0 # If max is Inf or -Inf due to all NAs
  }

  # Generate unique 5-digit code for sid
  # Ensure 'sid' column (the base district SID) exists before trying to paste to it.
  # Points that didn't intersect or find a nearest district might have NA for 'sid'.
  # We should filter them out or assign a default before generating the suffix.
  if (any(is.na(meta_data_copy$sid))) {
    warning("Some points could not be assigned a base district SID and will have NA for final SID.")
  }

  meta_data_copy <- meta_data_copy %>%
    dplyr::arrange(dplyr::desc(Lat)) %>% # Arrange for consistent SID assignment
    dplyr::mutate(
      # Only generate suffix if base sid is available
      sid_suffix = sprintf("%05d", md_start + dplyr::row_number()),
      sid = dplyr::if_else(!is.na(sid), paste0(sid, sid_suffix), NA_character_)
    ) %>%
    dplyr::select(-sid_suffix) # Remove temporary suffix column

  # Check for duplicate station names
  if(any(meta_data_copy$station_nm %in% md_stations$station_nm)){
    # Using yesno for interaction; for automated scripts, you might want a different default behavior
    if (!yesno(paste0('There are ',sum(meta_data_copy$station_nm %in% md_stations$station_nm > 0) ,' `station_nm` named the same as existing records.\n Do you want to continue?'))) {
      message("Operation cancelled by user due to duplicate station names.")
      return(invisible(NULL)) # Return NULL or original data
    }
  }

  # Get COMID for each station
  comids_nwis <- meta_data_copy %>%
    split(.$sid) %>%
    purrr::map_df(~dplyr::tibble(sid = .x$sid[1], COMID = comids(.x))) %>%
    dplyr::mutate(COMID = as.character(COMID)) # Use map_df to combine results easily

  # Join COMIDs back
  meta_data_final <- meta_data_copy  %>%
    dplyr::left_join(comids_nwis, by = "sid", suffix = c("", "_new")) %>%
    # Prefer newly generated COMID if old one was NA
    dplyr::mutate(COMID = dplyr::if_else(is.na(COMID), COMID_new, COMID)) %>%
    dplyr::select(-COMID_new) %>%
    sf::st_drop_geometry() %>%
    # Ensure region column exists and set it if it was NA
    dplyr::mutate(region = dplyr::if_else(is.na(region), 'Northern Region', region))

  # 3. Final Column Selection and Ordering
  # Ensure all target columns are present, and select them in the correct order.
  # This will drop any user-specific columns that were not mapped.
  final_output_df <- meta_data_final %>%
    dplyr::select(dplyr::all_of(hydb_target_cols))

  return(final_output_df)
}



#' Transform IV to DV or DV to IV
#'
#' @param data A data.frame
#'
#' @return A `tibble()`.
#' @export
#' @importFrom dplyr "%>%"
#' @note Need to have columns `dt`, `sid` and `iv_*` and `iv_*` needs to be column position 2.

hydb_daily_transform <- function(data) {


  data <- data %>%
    dt_to_tibble()

  if(any(startsWith(colnames(data), 'iv'))) {

  colname <- colnames(data)[startsWith(colnames(data), 'iv')]

  data_add_daily_stats <-  data %>%
    dplyr::mutate( date = lubridate::as_date(dt)) %>%
    dplyr::group_by(sid, date) %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(dplyr::starts_with('iv_')),
                     list(
                       sum = ~sum(.x, na.rm = TRUE),
                       max = ~max(.x, na.rm = TRUE),
                       min = ~min(.x, na.rm = TRUE),
                       mean = ~mean(.x, na.rm = TRUE),
                       median = ~median(.x, na.rm = TRUE),
                       stdev = ~sd(.x, na.rm = TRUE),
                       coef_var = ~sd(.x, na.rm = TRUE)/mean(.x, na.rm = TRUE))))  %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::all_of(c(colname, 'dt'))) %>%
    dplyr::relocate(date, dplyr::starts_with('iv_')) %>%
    tidyr::pivot_longer(dplyr::starts_with('iv_')) %>%
    dplyr::mutate(statistic_type_code = stat_cd(name))

  param_type <- paste0('dv',unique(substr(data_add_daily_stats$name, 3, 8)))

  variable_to_rename <- 'value'

  data_add_daily_stats %>%
  dplyr::rename(!!param_type := !!rlang::sym('value')) %>%
  dplyr::mutate(param = colname) %>%
  dplyr::select(-name)  %>%
  dplyr::mutate(statistic_type_code = stat_cd_to_name(statistic_type_code)) %>%
  tidyr::pivot_wider(values_from = dplyr::starts_with('dv_'), names_from = statistic_type_code)%>%
  dplyr::relocate(date, sid, param, sum:coef_var)

  } else if (any(startsWith(colnames(data), 'dv'))) {

  data %>%
  dplyr::mutate(statistic_type_code = ifelse(is.na(statistic_type_code), 'mean', stat_cd_to_name(statistic_type_code))) %>%
  tidyr::pivot_wider(values_from = dplyr::starts_with('dv_'), names_from = statistic_type_code)

  } else if (any(!is.na(match(colnames(data), c('sum', 'max', 'min', 'mean', 'median', 'stdev', 'coef_var'))))) {

    data %>%
      tidyr::pivot_longer(sum:coef_var) %>%
      dplyr::mutate(statistic_type_code = stat_cd(name))

  } else {

    message('Not used for tables other than iv_ or dv_')
  }

}


#' QA/QC table
#'
#' @param data A data.frame
#'
#' @return A `tibble()`.
#' @export
#' @importFrom dplyr "%>%"
#' @note Need to be a table from `hydb`.

hydb_qaqc <- function(data) {

  if(any(startsWith(colnames(data), 'iv'))) {

    data %>%
      dplyr::mutate(date = lubridate::as_date(dt)) %>%
      dplyr::group_by(sid, date) %>%
      dplyr::mutate(dplyr::across(dplyr::any_of(dplyr::starts_with('iv_')),
                                  list(
                                    iqr = ~quantile(.x, prob = 0.75) - quantile(.x, prob = 0.25),
                                    lb = ~quantile(.x, prob = 0.25) - 1.5*(quantile(.x, prob = 0.75) - quantile(.x, prob = 0.25)),
                                    ub = ~quantile(.x, prob = 0.75) + 1.5*(quantile(.x, prob = 0.75) - quantile(.x, prob = 0.25)),
                                    daily_z_score_outlier = ~ifelse(abs((.x - mean(.x, na.rm = TRUE))/sd(.x, na.rm = TRUE)) > 3, 'outlier', NA_character_),
                                    daily_iqr_outlier = ~ifelse(.x < quantile(.x, prob = 0.25) - 1.5*(quantile(.x, prob = 0.75) - quantile(.x, prob = 0.25)) |
                                                            .x > quantile(.x, prob = 0.75) + 1.5*(quantile(.x, prob = 0.75) - quantile(.x, prob = 0.25)),
                                                          'outlier', NA_character_)), .names = "{fn}"))  %>%
      #dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c('date', 'iqr', 'lb', 'ub'))

  } else if (any(startsWith(colnames(data), 'dv'))) {

    data %>%
      dplyr::mutate(statistic_type_code = stat_cd_to_name(statistic_type_code)) %>%
      tidyr::pivot_wider(values_from = dplyr::starts_with('dv_'), names_from = statistic_type_code)

  } else if (any(!is.na(match(colnames(data), c('sum', 'max', 'min', 'mean', 'median', 'stdev', 'coef_var'))))) {

    data %>%
      tidyr::pivot_longer(sum:coef_var) %>%
      dplyr::mutate(statistic_type_code = stat_cd(name))

  } else {

    message('Not used for tables other than iv_ or dv_')
  }

}
#' Convert dt to tibble
#'
#' @param data a data.frame and data.table
#' @noRd
#' @importFrom dplyr "%>%"
#' @return a tibble df
dt_to_tibble <- function(data) {

  if(any(class(data) %in% c("tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy"))) stop(message('Cannot perform on class "tbl_SQLiteConnection", "tbl_dbi", "tbl_sql", "tbl_lazy"'))

  class(data) <- 'data.frame'

  data <- dplyr::tibble(data)

  prefixes <- c('iv_', 'dv_', 'obs_', 'date', 'dt')

  matched_prefixes <- sapply(colnames(data), function(col) {
    match <- prefixes[grepl(paste(prefixes, collapse = '|'), col)]
    if(length(match) > 0) match else NA
  })


  colname1 <- names(matched_prefixes[!is.na(matched_prefixes)])[1]

  switch(sub('_.*', '', colname1),
        'dv' = data %>%
          dplyr::mutate(date = lubridate::as_date(date)),
        'date' = data %>%
          dplyr::mutate(date = lubridate::as_date(date)),
        'iv' = data %>%
          dplyr::mutate(dt = lubridate::as_datetime(dt)),
        'dt' = data %>%
          dplyr::mutate(dt = lubridate::as_datetime(dt)),
        'obs' = data %>%
          dplyr::mutate(date = lubridate::as_date(date)))

}

#' Conversions
#'
#' Convert Metrics
#' @param x data.frame
#' @param type character.
#'
#' @note When using 'c' for Celsius, this converts Fahrenheit to Celsius and vice versa for 'f'.
#'
#'
#' @return Side effect transformation on data.frame
#' @export
#'
hydb_conversions <- function(x, type) {

  switch(type,
         'c' = (x - 32) * (5/9),
         'f' = x * (9/5) + 32)

}

#' Stations in Tables
#'
#' @param data data.frame with station sid's or character vector of sid's.
#' @param hydbtables character. tables to select from, default 'all'.
#' @importFrom dplyr "%>%"
#'
#' @return Nothing. Side effect to `hydb`.
#' @export
#'

hydb_station_info <- function(data, hydbtables = 'all') {

  con <- .hydb_get_connection()


  if(any(class(data) %in% c('data.frame', 'tbl', 'tbl_df'))){

    stored_sid <- unique(data[['sid']])

  } else {

    stored_sid <- data

  }

  tables <- DBI::dbListTables(con)

  if(any(tables != 'all')){

    tables <- tables[tables %in% hydbtables]

  } else {

    tables <- tables[!tables %in% c('station_metadata', 'stat_codes', 'param_codes')]

  }

  station_info_func <- function(table, stored_sid) {

    final_data <- dplyr::tibble()

    for(i in table) {

    rows <- hydb_fetch(table = i, sid %in% stored_sid, collect = FALSE) %>%
                   dplyr::mutate(n = n()) %>%
                   utils::head(1) %>%
                   dplyr::pull(n)

    if(length(rows) > 0){

    info <- hydb_fetch(table = i, sid %in% stored_sid, collect = FALSE) %>% utils::head(1) %>%  dplyr::collect()

    colnames1 <- colnames(info)[!is.na(match(colnames(info), c('dt', 'date')))]


    years <- hydb_fetch(table = i, sid %in% stored_sid, collect = FALSE) %>%
        dplyr::group_by(year = lubridate::year(.data[[colnames1]])) %>%
        slice_hydb(1) %>%
        dplyr::ungroup() %>%
        dplyr::collect() %>%
        dt_to_tibble() %>%
        dplyr::mutate(year = lubridate::year(.data[[colnames1]])) %>%
        dplyr::pull(year)

    #years <- paste(years, collapse = ', ')

    } else {

    years <- NA_real_
    rows <- NA_real_

    }

    info_list <- dplyr::tibble(
                'table' = i,
                'count' = rows,
                'years' = list(years),
                'sid' = stored_sid
                )

    final_data <- dplyr::bind_rows(final_data, info_list)

    }


    final_data

  }

  final_list <- purrr::map(stored_sid,~station_info_func(tables,.x))

  if(any(class(data) %in% c('data.frame', 'tbl', 'tbl_df'))){

    dplyr::bind_rows(final_list) %>%
      dplyr::filter(!is.na(years)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
      min_year = min(years),
      max_year = max(years)) %>%
      dplyr::mutate(
        longest_streak = longest_streak(years)[[1]]-1,
        missing_years = ifelse(length(as.character(longest_streak(years)[[2]])) == 0,
                               NA_character_,
                               paste(as.character(longest_streak(years)[[2]]), collapse = ', ')),
        total_missing_years = length(as.character(longest_streak(years)[[2]]))
      ) %>%
      dplyr::ungroup() %>%
    dplyr::left_join(data, by = 'sid') %>%
      dplyr::rowwise() %>%
      dplyr::mutate(years = as.character(paste0(years, collapse = ', '))) %>%
      dplyr::ungroup()

  } else {

    dplyr::bind_rows(final_list) %>%
      dplyr::filter(!is.na(years)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
      min_year = min(years),
      max_year = max(years)) %>%
      dplyr::mutate(
        longest_streak = longest_streak(years)[[1]]-1,
        missing_years = ifelse(length(as.character(longest_streak(years)[[2]])) == 0,
                               NA_character_,
                               paste(as.character(longest_streak(years)[[2]]), collapse = ', ')),
        total_missing_years = length(as.character(longest_streak(years)[[2]]))
      ) %>%
      dplyr::ungroup() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(years = as.character(paste0(years, collapse = ', '))) %>%
      dplyr::ungroup()
  }
}



#' Find Closest Match
#'
#' @param text A character string
#' @param keys A character string
#'
#' @return A character string

find_closest_match <- function(text, keys) {

  cleaned_text <- gsub("[^a-zA-Z]", " ", gsub("_", " ", text))

  distances <- stringdist::stringdist(cleaned_text, keys, method = 'jw', p = 0.1)

  closest_match <- keys[which.min(distances)]

  as.character(closest_match)

}

#' Longest Streak
#'
#' @param num_list A list with numeric input.
#'
#' @return A list

longest_streak <- function(num_list) {
  if (length(num_list) == 0) return(list(longest_streak = 0, missing_numbers = integer(0)))

  num_list <- sort(unique(num_list))  # Sort and remove duplicates
  diff_seq <- c(1, diff(num_list))    # Compute differences

  # Identify streaks (consecutive numbers have diff of 1)
  streaks <- rle(diff_seq == 1)

  # Find the longest streak
  longest_streak <- ifelse(any(streaks$values),
                           max(streaks$lengths[streaks$values]) + 1, 1)

  # Identify missing numbers
  full_range <- seq(min(num_list), max(num_list))
  missing_numbers <- setdiff(full_range, num_list)

  return(list(longest_streak = longest_streak, missing_numbers = missing_numbers))
}

