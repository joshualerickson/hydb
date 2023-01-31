#' Fetch Data from Hydrological Database.
#'
#' @param table A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.
#' @param sid A character vector of station ids. NULL (default).
#' @param network logical. Whether to connect via the network or server. TRUE (default).
#'
#' @return A data.frame
#' @export
#'
fetch_hydb <- function(table,
                       sid = NULL,
                       network = TRUE) {



  if(network){
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
                        'metadata' = dplyr::collect(tbl(mydb, 'station_metadata'))
                      )

  if(!is.null(sid)){

  fetch_table <- fetch_table %>%
                 dplyr::filter(sid %in% sid)

  }

  DBI::dbDisconnect(mydb)

  return(fetch_table)

}
