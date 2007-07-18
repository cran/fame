\name{lines.tis}
\alias{lines.tis}
\alias{points.tis}
\title{Plotting Time Indexed Series}
\description{
  Plotting methods for \code{tis} objects
}
\usage{
\method{lines}{tis}(x, offset = 0.5, dropNA = FALSE, ...)
\method{points}{tis}(x, offset = 0.5, dropNA = FALSE, ...)
}
\arguments{
  \item{x}{
	a \code{tis} (time indexed series) object
  }
  \item{offset}{
	a number in the range [0,1] telling where in each period of \code{x}
	to plot the point. 0 means the first second of each period, 1 the
	last second of the period, and the default 0.5 plots each point in
	the middle of the time period in which it falls.
  }
  \item{dropNA}{ if \code{TRUE}, observations with \code{NA} values are
	dropped before calling \code{lines.default} or
	\code{points.default}. See the details for why you might or might
	not want to do this.  The default is \code{FALSE}, to match the
	behavior of \code{lines.default} and \code{points.default}.
  }
  \item{\dots}{
	other arguments to be passed on to \code{lines.default} or
	\code{points.default}. 
  }
}
\details{
  These are fairly simple wrappers around the \code{lines.default} and
  \code{points.default}.  For example, \code{lines.tis} basically does this:
  
  lines.default(x = time(x, offset = offset), y = x, ...)

  and \code{points.tis} is similar.  If \code{dropNA} is \code{TRUE}, the
  observations in \code{x} that are \code{NA} are dropped from the x and
  y vectors sent to the .default functions.  For \code{points}, this
  shouldn't matter, since \code{points.tis} omits points with \code{NA}
  values from the plot.

  For \code{lines} the \code{dropNA} parameter does make a difference.
  The help document for \code{lines} says:
  
  "The coordinates can contain \code{NA} values. If a point contains
  \code{NA} in either its \code{x} or \code{y} value, it is omitted from
  the plot, and lines are not drawn to or from such points.  Thus
  missing values can be used to achieve breaks in lines."

  Note that if the \code{type} is one of \code{c("p", "b", "o")}, the
  non-\code{NA} points are still drawn, but line segments from those
  points to adjacent \code{NA} points are not drawn.  If \code{dropNA = TRUE},
  the \code{NA} points are dropped before calling \code{lines.default},
  and all of the remaining points will be connected with line segments
  (unless suppressed by the \code{type} argument).
}
\author{ Jeff Hallman }
\seealso{ \code{\link{lines}}, \code{\link{points}}}
\keyword{ts}
\keyword{aplot}
