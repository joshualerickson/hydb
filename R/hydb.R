
#' Connect to hydb
#'
#' @param path A character vector file path to `.sqlite` database.
#' @return Nothing. Side effect connecting to hydb.
#' @export
#'
#' @note Must be a member of the `USDA Northern Region Hydrology` sharepoint group if `path = NULL`.
hydb_connect <- function(path = NULL) {
  # Check if a connection already exists in the package environment
  if (!is.null(.hydb_env$connection) && DBI::dbIsValid(.hydb_env$connection)) {
    message("An active hydb connection already exists. Disconnecting the old one first.")
    DBI::dbDisconnect(.hydb_env$connection)
    .hydb_env$connection <- NULL
    .hydb_env$db_file_path <- NULL
  }

  if (is.null(path)) {
    # Default path logic for Windows Sharepoint
    windows_path <- normalizePath(file.path(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH")), winslash = .Platform$file.sep)
    db_file_path <- file.path(windows_path, "USDA", "Northern Region Hydrology - Documents", "data-madness", "hydb", "hydb.sqlite")
  } else {
    db_file_path <- path
  }

  # Check if the database file exists
  if (!file.exists(db_file_path)) {
    stop(paste("Database file not found at:", db_file_path,
               "\nPlease ensure the path is correct or the file exists."))
  }

  # Establish new connection
  mydb <- DBI::dbConnect(RSQLite::SQLite(), db_file_path)

  # Store connection and its path in package environment
  .hydb_env$connection <- mydb
  .hydb_env$db_file_path <- db_file_path

  message(paste("Connected to hydb at:", db_file_path))
  invisible(mydb) # Return invisibly
}

#' Disconnect to hydb
#'
#' @return Nothing. Side effect connecting to hydb.
#' @export
#'
#' @note Must be a member of the `USDA Northern Region Hydrology` sharepoint group.
hydb_disconnect <- function() {
  if (!is.null(.hydb_env$connection) && DBI::dbIsValid(.hydb_env$connection)) {
    DBI::dbDisconnect(.hydb_env$connection)
    .hydb_env$connection <- NULL
    .hydb_env$db_file_path <- NULL
    message("Disconnected from hydb.")
  } else {
    message("No active hydb connection to disconnect.")
  }
  invisible(NULL)
}

#' @title Get Internal hydb Database Connection
#' @description
#' Retrieves the currently active `DBIConnection` object stored in the
#' internal `.hydb_env` environment.
#'
#' @details
#' This function is for internal package use only. It ensures that a valid
#' connection exists before returning it, otherwise it stops with an error.
#' Users should use `hydb_connect()` to establish a connection.
#'
#' @return A `DBIConnection` object.
#' @keywords internal
#' @rdname .hydb_env # Links this documentation to the same page as .hydb_env
.hydb_get_connection <- function() {
  if (is.null(.hydb_env$connection) || !DBI::dbIsValid(.hydb_env$connection)) {
    stop("No active hydb connection found. Please call `hydb_connect()` first.")
  }
  .hydb_env$connection
}
