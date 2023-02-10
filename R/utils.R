#' Get paramCd names
#'
#' @param x vector of parameter_cd
#' @return A vector of length 1 or n, matching the length of the logical input or output vectors.
#' @noRd
#'
name_params_to_update <- function(x){


  param_names <- dplyr::case_when(
    x == "discharge" ~ "value",
    x == "Flow" ~ "value",
    x == "flow" ~ "value",
    x == "q" ~ "value",
    x == "Q" ~ "value",
    x == "discharge_cfs" ~ "value",
    x == "date" ~ "date",
    x == "date_time" ~ "dt",
    x == "stage" ~ "value",
    x == "gage_height" ~ "value",
    x == "GH" ~ "value",
    x == "gh" ~ "value",
    x == "Date" ~ "date",
    x == "dt" ~ "dt",
    x == "time" ~ "time",
    x == "Time" ~ "time",
    grepl("h2o", x) ~ "value",
    grepl("h2o_temp", x) ~ "value",
    grepl("water_temp", x) ~ "value",
    grepl("air", x) ~ "value",
    grepl("air_temp", x) ~ "value",
    grepl("air_temperature", x) ~ "value",
    TRUE ~ x)

}
