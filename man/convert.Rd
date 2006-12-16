\name{convert}
\alias{convert}
\title{
  Time scale conversions for time series
}
\description{
  Convert \code{tis} series from one frequency to another using a
  variety of algorithms.
}
\usage{
convert(x, tif, method = "constant", observed. = observed(x),
        basis. = basis(x), ignore = F)
}
\arguments{
  \item{x}{
	a univariate or multivariate \code{tis} series. Missing values (NAs)
	are ignored. 
  }
  \item{tif}{
	a number or a string indicating the desired ti frequency of the
	return series. See \code{help(ti))} for details.
  }
  \item{method}{
	method by which the conversion is done: one of "discrete",
	"constant", "linear", or "cubic".
  }
  \item{observed.}{
	"observed" attribute of the input series: one of "beginning", "end",
	"summed", "annualized", or "averaged".  If this argument is not
	supplied and observed(\code{x}) != NULL it will be used.  The output
	series will also have this "observed" attribute.
  }
  \item{basis.}{
	"daily" or "business".  If this argument is not supplied and
	basis(\code{x}) != NULL it will be used. The output series will also
	have this "basis" attribute. 
  }
  \item{ignore}{
	governs how missing (partial period) values at the beginning and/or
	end of the series are handled.  For method == "discrete" or
	"constant" and ignore == T, input values that cover only part the
	first and/or last output time intervals will still result in output
	values for those intervals.  This can be problematic, especially for
	observed == "summed", as it can lead to atypical values for the
	first and/or last periods of the output series.
  }
}
\value{
  a \code{tis} time series covering approximately the same time span as
  \code{x}, but with the frequency specified by \code{tif}.
}
\details{
  This function is a close imitation of the way FAME handles time scale
  conversions.  See the chapter on "Time Scale Conversion" in the Users
  Guide to Fame if the explanation given here is not detailed enough.

  Start with some definitions.  Combining values of a higher frequency
  input series to create a lower frequency output series is known as
  \code{aggregation}. Doing the opposite is known as \code{disaggregation}.

  Disaggration for "discrete" series: (i) for observed == "beginning"
  ("end"), the first (last) output period that begins (ends) in a
  particular input period is assigned the value of that input period.
  All other output periods that begin (end) in that input period are NA.
  (ii) for observed == "summed" or "averaged", all output periods that
  end in a particular input period are assigned the same value.  For
  "summed", that value is the input period value divided by the number
  of output periods that end in the input period, while for an
  "averaged" series, the output period values are the same as the
  corresponding input period values.  

  Aggregation for "discrete" series: (i) for observed == "beginning"
  ("end"), the output period is assigned the value of the first (last)
  input period that begins (ends) in the output period. (ii) for
  observed == "summed" ("averaged"), the output value is the sum
  (average) of all the input values for periods that end in the output
  period. 

  Methods "constant", "linear", and "cubic" all work by constructing a
  continuous function F(t) and then reading off the appropriate
  point-in-time values if observed == "beginning" or "end", or by
  integrating F(t) over the output intervals when observed == "summed",
  or by integrating F(t) over the output intervals and dividing by the
  lengths of those intervals when observed == "averaged".  The unit of
  time itself is given by the \code{basis} argument. 

  The form of F(t) is determined by the conversion method. For
  "constant" conversions, F(t) is a step function with jumps at the
  boundaries of the input periods.  If the first and/or last input
  periods only partly cover an output period, F is linearly extended to
  cover the first and last output periods as well.  The heights of the
  steps are set such that F(t) aggregates over the input periods to the
  original input series. 

  For "linear" ("cubic") conversions, F(t) is a linear (cubic) spline.
  The x-coordinates of the spline knots are the beginnings or ends of
  the input periods if observed == "beginning" or "end", else they are
  the centers of the input periods. The y-coordinates of the splines are
  chosen such that aggregating the resulting F(t) over the input periods
  yields the original input series. 

  For "constant" conversions, if \code{ignore} == F, the first (last)
  output period is the first (last) one for which complete input data is
  available.  For observed == "beginning", for example, this means that
  data for the first input period that begins in the first output period
  is available, while for observed == "summed", this means that the
  first output period is completely contained within the available input
  periods.  If \code{ignore} == T, data for only a single input period
  is sufficient to create an output period value.  For example, if
  converting weekly data to monthly data, and the last observation is
  June 14, the output series will end in June if \code{ignore} == T, or
  May if it is F. 

  Unlike the "constant" method, the domain of F(t) for "linear" and
  "cubic" conversions is NOT extended beyond the input periods, even if
  the ignore option is T. The first (last) output period is therefore
  the first (last) one that is completely covered by input periods.

  Series with observed == "annualized" are handled the same as observed
  == "averaged".
}
\section{BUGS}{
  Method "cubic" is not currently implemented for observed "summed",
  "annualized", and "averaged". 
}
\author{ Jeff Hallman}
\references{Users Guide to Fame}
\seealso{
\code{\link{aggregate}}, \code{\link{tif}}, \code{\link{ti}}}
\examples{
wSeries <- tis(1:105, start = ti(19950107, tif = "wsaturday"))
observed(wSeries) <- "ending"   ## end of week values
mDiscrete <- convert(wSeries, "monthly", method = "discrete")
mConstant <- convert(wSeries, "monthly", method = "constant")
mLinear   <- convert(wSeries, "monthly", method = "linear")
mCubic    <- convert(wSeries, "monthly", method = "cubic")

## linear and cubic are identical because wSeries is a pure linear trend
cbind(mDiscrete, mConstant, mLinear, mCubic)

observed(wSeries) <- "averaged"   ## weekly averages
mDiscrete <- convert(wSeries, "monthly", method = "discrete")
mConstant <- convert(wSeries, "monthly", method = "constant")
mLinear   <- convert(wSeries, "monthly", method = "linear")

cbind(mDiscrete, mConstant, mLinear)
}
\keyword{ts}
