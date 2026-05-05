#' Append DB Table
#'
#' @param data data.frame to append.
#' @param table character. DB table to append to.
#' @param check logical. Checking details before appending.
#' @param copy_path logical. Whether to make a copy of db.
#' @param path_to_copy character. Where to write the copy of db if `copy = TRUE`.
#'
#' @return Nothing. Side effect to `hydb`.
#' @importFrom dplyr "%>%"
#' @export
#'

hydb_append_table <- function(data,
                              table = NULL,
                              check = TRUE,
                              copy_path = TRUE,
                              path_to_copy = NULL) {

  stored_sid <- unique(data[['sid']])

  existing_data <- hydb_fetch(table = table, sid %in% stored_sid)

  # Get the new rows that are not duplicates
  if(!is.null(existing_data)){

  new_rows <- dplyr::anti_join(data, existing_data)

    print(sprintf("%d new rows added to %s.", nrow(new_rows), table))

    print(sprintf("%d sid's for %s.", length(unique(new_rows$sid)), table))

    station_stuff <- hydb_fetch(table = 'station_metadata') %>%
      dplyr::filter(sid %in% unique(new_rows$sid)) %>%
      dplyr::group_by(sid) %>%
      dplyr::slice(1)


    print(sprintf("sid: %s, station_nm: %s", unique(station_stuff$sid), station_stuff %>%
                    dplyr::pull(station_nm)))
  } else {

   print('New data added, so no info')

   new_rows <- data

  }

  if(!check) {

    con <- .hydb_get_connection()

    # create a copy before adding
  if(copy_path){

    if(is.null(path_to_copy)){

      windows_path <- normalizePath(file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")), winslash = .Platform$file.sep)

      path_to_copy <- file.path('/USDA/Northern Region Hydrology - Documents/data-madness/hydb')


    } else {

    db_path <- paste0(path_to_copy, '/copy_hydb.sqlite')

    RSQLite::sqliteCopyDatabase(con,db_path)

    }

    }

    # now append
    DBI::dbAppendTable(con, name = table, value = new_rows)


    }

}

#' @title Slice for tbl_sql
#' @param .data tbl_sql class
#'
#' @param ... dots to pass to `dplyr` verbs
#' @importFrom dplyr "%>%"
#'
slice_hydb <- function(.data, ...) {
  rows <- c(...)

  .data %>%
    dplyr::mutate(...row_id = dplyr::row_number()) %>%
    dplyr::filter(...row_id %in% !!rows) %>%
    dplyr::select(-...row_id)

}

#' Convert to SQL dates
#' @param condition A list
#'
#' @return A list with updated date syntax for SQL.
#'
convert_dates <- function(condition) {
  if ("date" %in% names(condition)) {
    condition[["date"]] <- as.integer(as.Date(condition[["date"]], format = "%Y-%m-%d"))
  } else if ("date_time" %in% names(condition)) {
    condition[["date_time"]] <- as.integer(as.Date(condition[["date_time"]], format = "%Y-%m-%d hh:mm:ss"))
  } else if ("date_time" %in% names(condition)) {
    condition[["time"]] <- as.integer(as.Date(condition[["time"]], format = "hh:mm:ss"))
  }
  return(condition)
}

