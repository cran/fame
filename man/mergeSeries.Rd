\name{mergeSeries}
\alias{mergeSeries}
\title{ Merge Time Indexed Series }
\description{
  Merge two time-indexed series using either the levels or the first differences
  of the second series where the series overlap.
}
\usage{
mergeSeries(x, y, differences = F)
}
\arguments{
  \item{x, y}{
	\code{tis} objects, or objects that can sensibly be coerced to
	\code{tis} by \code{as.tis}.
  }
  \item{differences}{
	if \code{T}, the first differences of series are merged, and then
	cumulatively summed. The default is \code{F}.
  }
}
\details{
  \code{x} and \code{y} must have the same \code{tif} (ti frequency),
  and the same number of column (if they are multivariate). 
}
\value{
  A \code{tis} object series with start and end dates that span those of
  \code{x} and \code{y}.  Where the series overlap, values from \code{y}
  are used. 
}
\author{ Jeff Hallman }
\seealso{ \code{\link{cbind.tis}} }
\keyword{ ts }
