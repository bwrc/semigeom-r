% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{get_function_list}
\alias{get_function_list}
\title{Get tiling functions.}
\usage{
get_function_list()
}
\arguments{
\item{X}{The data matrix.}

\item{K}{The maximum number of tiles. Default is 1.}

\item{alpha}{The precision threshold. Default is 0.75.}
}
\value{
All tiles with a precision of at least alpha.
}
\description{
Get the MaxTile and GlobalTile functions as a list.
}

