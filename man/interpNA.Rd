\name{interpNA}
\alias{interpNA}
\title{ Interpolate missing values in a Time Indexed Series }
\description{ Calls \code{\link{approxfun}} or \code{\link{splinefun}}
  to interpolate missing values in a \code{\link{tis}} object.
}
\usage{
interpNA(x, method = "constant", useTimes = F, offset = 1, rule = 2, f = 0, ...)
}
\arguments{
  \item{x}{a \code{tis} time series}
  \item{method}{One of c("constant", "linear", "fmm", "natural",
	"periodic").  Methods "constant" and "linear" call \code{approxfun};
	the others call \code{splinefun}.
  }
  \item{useTimes}{if \code{TRUE}, use \code{time(x, offset)} (the
	decimal times of \code{x}) as the 'x' part of the (x, y) pairs used for
	interpolation.  If \code{FALSE} (the default), use \code{ti(x)} (the
	integer time indices of \code{x}) as the 'x' part of the (x, y) pairs.
  }
  \item{offset}{if \code{useTimes} is \code{TRUE}, a number in the range
	[0,1] telling where in the periods represented by \code{ti(x)} to
	get the points for the 'x' parts of the (x, y) pairs. See the help
	for \code{\link{jul}} for a more detailed explanation of this parameter.
  }
  \item{rule}{For methods "constant" and "linear": an integer describing
	how interpolation is to take place outside the interval
	[\code{min(x)}, \code{max(x)}]. If \code{rule} is \code{1} then
	\code{NA}s are returned for such points and if it is \code{2}, the
	value at the closest data extreme is used.
  }
  \item{f}{For \code{method="constant"} a number between 0 and 1
    inclusive, indicating a compromise between left- and
    right-continuous step functions. If \code{y0} and \code{y1} are the
    values to the left and right of the point then the value is
    \code{y0*(1-f)+y1*f} so that \code{f=0} is right-continuous and
    \code{f=1} is left-continuous.
  }
  \item{\dots}{Other arguments passed along to \code{approxfun} for
	methods "constant" and "linear".}
}
\details{Depending on the method specified, a call to either
  \code{approxfun} or \code{splinefun} is constructed with appropriate
  arguments and executed for each column of \code{x}.  In the call to
  \code{approxfun} or \code{splinefun}, the time indices \code{ti(x)}
  (or the decimal times returned by \code{time(x, offset)}, if
  \code{useTimes} is \code{TRUE}) serve as the 'x' argument and the
  column values as the 'y' argument.
}
\value{ A \code{tis} object like \code{x} with \code{NA} values filled
  in by interpolated values.}
\author{ Jeff Hallman }
\seealso{ \code{\link{approxfun}}, \code{\link{splinefun}},
  \code{\link{ti}}}
\keyword{ dplot }
\keyword{ ts }
