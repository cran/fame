\name{lines.tis}
\alias{lines.tis}
\alias{points.tis}
\title{Plotting Time Indexed Series}
\description{
  Plotting methods for \code{tis} objects
}
\usage{
\method{lines}{tis}(x, midPoints = T, offset = 1, dropNA = F, ...)
\method{points}{tis}(x, midPoints = T, offset = 1, dropNA = F, ...)
}
\arguments{
  \item{x}{
	a \code{tis} (time indexed series) object
  }
  \item{midPoints}{
	if \code{TRUE} (the default) and if \code{offset} was
	not specified, then \code{offset} is set to 0.5.
  }
  \item{offset}{
	a number in the range [0,1] telling where in each period of \code{x}
	to find the day on which to plot the point. 0 means the first 
	day of each period; the default value 1 plots on the last day of
	each period.  For example, if \code{x} has \code{tif} = "wmonday" so
	that each observation represents a week ending on Monday, than any
	\code{offset} in the range [0, 1/7] will plot points on Tuesdays, 
	while \code{offset} in the range (1/7, 2/7] will plot on Wednesdays,
	\code{offset} in the range (6/7, 1] will plot on Mondays, and so on.
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
