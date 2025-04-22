#' Fetch Data from Hydrological Database.
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param new_values A list of column and value to change for SQL.
#' @param conditions A list of conditions to be met for SQL.
#' @param network NULL. Used in shiny application.
#'
#' @note A list
#' @return Nothing.
#' @export
#'
update_hydb <- function(table,
                        new_values,
                        conditions,
                        network = NULL) {

  if(missing(new_values)) {stop("Need Values to Change")}
  if(missing(conditions)) {stop("Need Conditions")}

  if(!is.null(network)){

    mydb <- network

  } else {

    mydb <- hydb_connect()

  }

  uptbl(table, new_values, conditions, mydb)



}


#' Update Table
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param new_values A list of column and value to change for SQL.
#' @param conditions A list of conditions to be met for SQL.
#' @param mydb A database object
#'
#' @return Nothing. This function updates the database
uptbl <- function(table, new_values, conditions, mydb) {

  conditions <- lapply(conditions, convert_dates)

 j = 0

 for (i in conditions) {

   # set counter for new values
    j = j + 1

    set_clauses <- sapply(names(new_values[[j]]), function(col) {
      sprintf("%s = '%s'", col, new_values[[j]][[col]])
    })

    set_clause <- paste(set_clauses, collapse = ", ")


    condition_clauses <- sapply(names(i), function(col) {
      sprintf("%s = '%s'", col, i[[col]])
    })

    condition_str <- paste(condition_clauses, collapse = " AND ")

    update_query <- sprintf("UPDATE %s SET %s WHERE %s",
                          table, set_clause, condition_str)

     DBI::dbExecute(mydb, update_query)
 }

}

#' Append DB Table
#'
#' @param data data.frame to append.
#' @param table character. DB table to append to.
#' @param check logical. Checking details before appending.
#' @param copy_path logical. Whether to make a copy of db.
#' @param path_to_copy character. Where to write the copy of db if `copy = TRUE`.
#' @param ... Arguments to pass to `hydb_connect()`.
#'
#' @return Nothing. Side effect to `hydb`.
#' @importFrom dplyr "%>%"
#' @export
#'

hydb_append_table <- function(data,
                              table = NULL,
                              check = TRUE,
                              copy_path = TRUE,
                              path_to_copy = NULL,
                              ...) {

  stored_sid <- unique(data[['sid']])

  existing_data <- hydb_fetch(table = table, sid %in% stored_sid, collect = FALSE) %>%
                   dplyr::collect() %>%
                   dt_to_tibble()

  # Get the new rows that are not duplicates
  if(!is.null(existing_data)){

  new_rows <- dplyr::anti_join(data, existing_data)

    print(sprintf("%d new rows added to %s.", nrow(new_rows), table))

    print(sprintf("%d sid's for %s.", length(unique(new_rows$sid)), table))

    station_stuff <- hydb_fetch('station_metadata', collect = FALSE) %>%
      dplyr::collect() %>%
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

    # create a copy before adding
  if(copy_path){

    if(is.null(path_to_copy)){

      windows_path <- normalizePath(file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")), winslash = .Platform$file.sep)

      path_to_copy <- file.path('/USDA/Northern Region Hydrology - Documents/data-madness/hydb')


    } else {

    db_path <- paste0(path_to_copy, '/copy_hydb.sqlite')

    RSQLite::sqliteCopyDatabase(hydb_connect(...),db_path)

    }

    }

    # now append
    DBI::dbAppendTable(hydb_connect(...), name = table, value = new_rows)


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

