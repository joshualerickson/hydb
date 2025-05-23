% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.R
\name{hydb_fetch}
\alias{hydb_fetch}
\title{Fetch Data from Hydrological Database.}
\usage{
hydb_fetch(table, ..., network = NULL, mydb_path = NULL, collect = TRUE)
}
\arguments{
\item{table}{A character vector of table names, e.g. 'station_metadata', 'flow_dv', etc.}

\item{...}{Option to pass calls to \code{dplyr::filter()}.}

\item{network}{NULL. Used in shiny application.}

\item{mydb_path}{A connection object to a .sqlite db.}

\item{collect}{A logical indicating whether to \link[dplyr]{collect} the table, default (TRUE).}
}
\value{
A data.frame
}
\description{
Fetch Data from Hydrological Database.
}
