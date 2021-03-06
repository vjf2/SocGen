% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_pairs.R
\name{merge_pairs}
\alias{merge_pairs}
\title{A Function to Merge Pairs}
\usage{
merge_pairs(
  x,
  y,
  xID1,
  xID2,
  yID1 = xID1,
  yID2 = xID2,
  all.x = TRUE,
  all.y = FALSE
)
}
\arguments{
\item{x}{a matrix or dataframe}

\item{y}{a matrix or dataframe}

\item{xID1, xID2, yID1, yID2}{specifications of the columns used for merging}

\item{all.x, all.y}{logical, see merge function for details}
}
\description{
This function allows you to merge data based pairs of IDs that can be in either order
}
\examples{
mhead()
}
\keyword{merge}
\keyword{pairs}
