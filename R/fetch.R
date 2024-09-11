#' Fetch Data from Hydrological Database.
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param sid A character vector of station ids. NULL (default).
#' @param network NULL. Used in shiny application.
#' @param tbl_only A logical indicating whether to \link[dplyr]{collect} the table. default (FALSE).
#' @importFrom dplyr tbl
#'
#' @return A data.frame
#' @export
#'
fetch_hydb <- function(table,
                       sid = NULL,
                       network = NULL,
                       tbl_only = FALSE) {



  if(!is.null(network)){

   mydb <- network

  } else {

   mydb <- hydb_connect()

  }

  fetch_table <- ftbl(table, sid, mydb, tbl_only)

  fetch_table

}


#' Fetch Table
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param sid A character vector of station ids. NULL (default).
#' @param tbl_only A logical indicating whether to \link[dplyr]{collect} the table.
#' @param mydb A connected database object.
#'
#' @return A table
ftbl <- function(table, sid, mydb, tbl_only) {

  fetch_table <- switch(table,
         'flow_iv' = tbl(mydb, 'flow_iv'),
         'flow_dv' = tbl(mydb, 'flow_dv'),
         'flow_obs' = tbl(mydb, 'flow_obs'),
         'tss_iv' = tbl(mydb, 'tss_iv'),
         'tss_dv' = tbl(mydb, 'tss_dv'),
         'tss_obs' = tbl(mydb, 'tss_obs'),
         'precip_iv' = tbl(mydb, 'precip_iv'),
         'precip_dv' = tbl(mydb, 'precip_dv'),
         'precip_obs' = tbl(mydb, 'precip_obs'),
         'stage_iv' = tbl(mydb, 'stage_iv'),
         'stage_dv' = tbl(mydb, 'stage_dv'),
         'stage_obs' = tbl(mydb, 'stage_obs'),
         'airtemp_iv' = tbl(mydb, 'airtemp_iv'),
         'airtemp_dv' = tbl(mydb, 'airtemp_dv'),
         'airtemp_obs' = tbl(mydb, 'airtemp_obs'),
         'wtemp_iv' = tbl(mydb, 'wtemp_iv'),
         'wtemp_dv' = tbl(mydb, 'wtemp_dv'),
         'wtemp_obs' = tbl(mydb, 'wtemp_obs'),
         'station_metadata' = tbl(mydb, 'station_metadata'),
         'stat_codes' = tbl(mydb, 'stat_codes'),
         'param_codes' = tbl(mydb, 'param_codes'),
         'swidth_obs' = tbl(mydb, 'swidth_obs'),
         'svel_obs' = tbl(mydb, 'svel_obs'),
         'sarea_obs' = tbl(mydb, 'sarea_obs')
  )

  if(tbl_only) {

  } else {

  if(!is.null(sid)){

    fetch_table <- fetch_table %>%
      dplyr::filter(sid %in% {{sid}}) %>%
      dplyr::collect()

  } else {

    fetch_table <- fetch_table %>%
      dplyr::collect()
  }

   if(!grepl(c('metadata|codes'), table)){

    fetch_table <- switch(sub('.*\\_', '', table),
                          'dv' = fetch_table %>%
                            dplyr::mutate(date = lubridate::as_date(date)),
                          'iv' = fetch_table %>%
                            dplyr::mutate(dt = lubridate::as_datetime(dt)),
                          'obs' = fetch_table %>%
                            dplyr::mutate(date = lubridate::as_date(date)))


   }

    DBI::dbDisconnect(mydb)

  }

  return(fetch_table)
}

#' hydb Table List
#'
#' @param network  NULL. Used in shiny application.
#'
#' @return A list of tables in the hydb database.
#' @export
#'
#'
hydb_tables <- function(network = NULL) {

  if(!is.null(network)){

    mydb <- network

  } else {


    mydb <- hydb_connect()

  }

  DBI::dbListTables(mydb)

}
