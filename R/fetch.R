#' Fetch Data from Hydrological Database.
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param sid A character vector of station ids. NULL (default).
#' @param network NULL. Used in shiny application.
#' @importFrom dplyr tbl
#'
#' @return A data.frame
#' @export
#'
fetch_hydb <- function(table,
                       sid = NULL,
                       network = NULL) {



  if(!is.null(network)){

  mydb <- network

  } else {

  path <- 'T:/FS/NFS/Kootenai/Program/2500Watershed/GIS/SO/hydb'

  mydb <- DBI::dbConnect(RSQLite::SQLite(), paste0(path,"/hydb.sqlite"))

  }

  fetch_table <- switch(table,
                        'flow_iv' = dplyr::collect(tbl(mydb, 'flow_iv')),
                        'flow_dv' = dplyr::collect(tbl(mydb, 'flow_dv')),
                        'flow_obs' = dplyr::collect(tbl(mydb, 'flow_obs')),
                        'tss_iv' = dplyr::collect(tbl(mydb, 'tss_iv')),
                        'tss_dv' = dplyr::collect(tbl(mydb, 'tss_dv')),
                        'tss_obs' = dplyr::collect(tbl(mydb, 'tss_obs')),
                        'precip_iv' = dplyr::collect(tbl(mydb, 'precip_iv')),
                        'precip_dv' = dplyr::collect(tbl(mydb, 'precip_dv')),
                        'precip_obs' = dplyr::collect(tbl(mydb, 'precip_obs')),
                        'stage_iv' = dplyr::collect(tbl(mydb, 'stage_iv')),
                        'stage_dv' = dplyr::collect(tbl(mydb, 'stage_dv')),
                        'stage_obs' = dplyr::collect(tbl(mydb, 'stage_obs')),
                        'airtemp_iv' = dplyr::collect(tbl(mydb, 'airtemp_iv')),
                        'airtemp_dv' = dplyr::collect(tbl(mydb, 'airtemp_dv')),
                        'airtemp_obs' = dplyr::collect(tbl(mydb, 'airtemp_obs')),
                        'wtemp_iv' = dplyr::collect(tbl(mydb, 'wtemp_iv')),
                        'wtemp_dv' = dplyr::collect(tbl(mydb, 'wtemp_dv')),
                        'wtemp_obs' = dplyr::collect(tbl(mydb, 'wtemp_obs')),
                        'metadata' = dplyr::collect(tbl(mydb, 'station_metadata'))
                      )

  if(!is.null(sid)){

  fetch_table <- fetch_table %>%
                 dplyr::filter(sid %in% {{sid}})

  }

  DBI::dbDisconnect(mydb)

  if(!grepl('metadata', table)){

    fetch_table <- switch(sub('.*\\_', '', table),
           'dv' = fetch_table %>%
                  dplyr::mutate(date = lubridate::as_date(date)),
           'iv' = fetch_table %>%
                  dplyr::mutate(dt = lubridate::as_datetime(dt)),
           'obs' = fetch_table %>%
                   dplyr::mutate(date = lubridate::as_date(date)))


  }

  return(fetch_table)

}
