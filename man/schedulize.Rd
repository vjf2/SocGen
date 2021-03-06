% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/schedulize.R
\name{schedulize}
\alias{schedulize}
\title{Schedulize}
\usage{
schedulize(
  data,
  id = "dolphin_id",
  start = "entry",
  end = "depart",
  dates = dates,
  format = c("sim", "mask")
)
}
\arguments{
\item{data}{a dataframe}

\item{id}{individual IDs}

\item{start}{first date of availability}

\item{end}{last date of availability}

\item{dates}{dates in standard character or Date format}

\item{format}{one of "sim" for SENM or "mask" for calculating association indices}
}
\description{
This function converts a dataframe of entry and depart dates to an availability matrix for simulations or calculating association indices
}
\examples{
fast_random_points()
simple_ratio()
}
\keyword{availabilty}
\keyword{matrix}
\keyword{schedule}
