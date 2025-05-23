% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{clean_vec}
\alias{clean_vec}
\title{'Clean' a character/factor vector like \code{janitor::clean_names()} does for data frame columns}
\usage{
clean_vec(x, refactor = FALSE, dupe_count = FALSE)
}
\arguments{
\item{x}{a vector of strings or factors}

\item{refactor}{if \code{x} is a factor, return a ref-factored factor? Default: \code{FALSE} == return character vector.}

\item{dupe_count}{logical.}
}
\value{
A vector with clean names
}
\description{
Most of the internals are from \code{janitor::clean_names()}
}
