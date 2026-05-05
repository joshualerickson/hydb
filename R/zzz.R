#' @title Internal hydb Package Environment
#' @description
#' This environment is used internally by the `hydb` package to store global state,
#' primarily the active database connection and its associated file path.
#'
#' @details
#' It is not intended for direct user access. Access to the database connection
#' should be managed through `hydb_connect()`, `hydb_disconnect()`, and
#' internal helper functions like `.hydb_get_connection()`.
#'
#' \describe{
#'   \item{`connection`}{A `DBIConnection` object (typically `SQLiteConnection`) representing
#'     the active connection to the hydb database, or `NULL` if no connection is active.}
#'   \item{`db_file_path`}{A character string indicating the file path of the
#'     currently connected SQLite database, or `NULL`.}
#' }
#'
#' @keywords internal
#' @name .hydb_env
NULL # This NULL is important for documenting environments or variables without a function

# This environment will hold the active database connection
.hydb_env <- new.env(parent = emptyenv())
.hydb_env$connection <- NULL
.hydb_env$db_file_path <- NULL
