#' Fetch Data from Hydrological Database.
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param new_values A list of column and value to change for SQL.
#' @param conditions A list of conditions to be met for SQL.
#' @param network NULL. Used in shiny application.
#' @importFrom dplyr tbl
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

#' Convert to SQL dates
#' @param condition A list
#'
#' @return A list with updated date syntax for SQL.
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

