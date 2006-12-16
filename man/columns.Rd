\name{columns}
\alias{columns}
\alias{rows}
\title{Rows and Columns of a Matrix}
\description{Create lists from the rows and/or columns of a matrix.
}
\usage{
columns(z)
rows(z)
}
\arguments{
  \item{z}{a matrix}
}
\value{
  \code{rows} returns a list of the rows of \code{z}.  If \code{z} has
  row names, those will also be the names of the returned list.

  \code{columns} does the same, but for columns.  Note that if \code{z}
  is some kind of time series, so too will be the elements of the
  returned list.
}
\author{ Jeff Hallman }
\keyword{ manip }
\keyword{ list }
