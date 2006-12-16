\name{linearSplineIntegration}
\alias{ilspline}
\alias{lintegrate}
\title{Linear Spline Integration}
\description{
  \code{lintegrate} gives the values resulting from integrating a linear
  spline, while \code{ilspline} returns linear splines that
  integrate to given values. 
}
\usage{
lintegrate(x, y, xint, stepfun = F, rule = 0)
ilspline(xint, w)
}
\arguments{
\item{x}{x coordinates of the linear spline F defined by \code{(x, y)}}
\item{y}{y coordinates of the linear spline F defined by \code{(x, y)}}
\item{xint}{x intervals, i.e. \code{[x[1], x[2]]} is the first interval,
  \code{[x[2], x[3]]} is the second interval, and so on.}
\item{stepfun}{if \code{TRUE},  F is a left-continuous step function.
  The default (\code{FALSE}) says F is continuous.}
\item{rule}{one of \{0, 1, NA\} to specify the behavior of F outside the
   range of \code{x}.  Use zero if F is zero outside the range of \code{x},
   \code{NA} if F is NA outside the range of \code{x}, and one if F is to
   be linearly extended outside the range of \code{x}.}
  \item{w}{values the linear spline must integrate to}
}
\details{
  \code{lintegrate} integrates the linear spline F defined by
  \code{(x,y)} over the \code{xint} intervals.  The value of F outside
  the range of \code{x} is specified by the \code{rule} argument:
\preformatted{
    rule == 0  --> F(z) = 0 for z outside the range of x
    rule == NA --> F(z) = NA for z outside the range of x
    rule == 1  --> F(z) extended for z outside the range of x
}

  If \code{stepfun} is \code{TRUE}, F(z) is assumed to be a
  left-continuous step function and the last value of \code{y} is never
  accessed.

  \code{(x[i], y[i])} pairs with NA values in either x[i] or y[i] NA are
  ignored in constructing F.

  \code{ilspline} finds linear splines that integrate over the N
  intervals specified by the monotonically increasing N+1 vector
  \code{xint} to the N values given in \code{w}.  The function finds
  N-vectors x and y such that:

\preformatted{
  (i)  x[j] = (xint[j-1] + xint[j])/2, i.e., the values of x are the
       midpoints of the intervals specified by xint, and

  (ii) the linear spline that passes through the (x[i], y[i]) pairs (and
       is extended to xint[1] and xint[N+1] by linear
       extrapolation) integrates over each interval [xint[j],xint[j+1]]
       to w[j]. 
}
  In fact, \code{w} can actually be an M by N matrix, in which case the
  y found by the function is also an M by N matrix, with each column of
  y giving the y coordinates of a linear spline that integrates to the
  corresponding column of \code{w}.
}
\value{
  \code{lintegrate} returns a vector of length \code{length(xint) - 1}.

  \code{ilspline} returns a list with components named 'x' and 'y'.
}
\references{ ~put references to the literature/web site here ~ }
\author{ Jeff Hallman }
\seealso{ \code{\link{spline}}, \code{\link{approx}}}
\examples{
w <- 10 + cumsum(rnorm(10))
blah <- ilspline(1:11, w)
ww <- lintegrate(blah$x, blah$y, 1:11, rule = 1)
w - ww  ## should be all zeroes (or very close to zero)
}
\keyword{ math }
