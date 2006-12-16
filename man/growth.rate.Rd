\name{growth.rate}
\alias{growth.rate}
\alias{growth.rate<-}
\title{Growth Rates of Time Series}
\description{Get or set growth rates of a \code{tis} time series in annual
  percent terms.
}
\usage{
growth.rate(x, lag = 1, simple = T)
growth.rate(x, start = end(x) + 1, simple = T) <- value
}
\arguments{
  \item{x}{a \code{tis} time series or something that can be turned into
  one by \code{as.tis}}
  \item{lag}{number of lags to use in calculating the growth rate as
  outlined in the details below}
  \item{simple}{simple growth rates if \code{TRUE}, compound growth
	rates if \code{FALSE}}
  \item{start}{the first \code{ti} time index for which values of
  \code{x} should be replaced to make \code{growth.rate(x[start]) == value[1]}.}
  \item{value}{desired growth rates}
}
\details{
  An example: Suppose \code{x} is a quarterly series, then if \code{simple} is
  \code{TRUE}, 
  
  \code{growth.rate(x, lag = 3)} == \code{100 * ((x[t]/x[t-3]) - 1) * (4/3)}

  while if \code{simple} is \code{FALSE}
  
  \code{growth.rate(x, lag = 3)} == \code{100 * ((x[t]/x[t-3])^(4/3) - 1)}.
}

\value{
  \code{growth.rate(x)} returns a \code{tis} series of growth rates in
  annual percentage terms.

  Beginning with the observation indexed by \code{start},
  
  \code{growth.rate(x) <- value}

  sets the values of \code{x} such that the growth rates in annual
  percentage terms will be equal to \code{value}. \code{x} is extended
  if necessary. The modified \code{x} is invisibly returned. 
}
\author{ Jeff Hallman }
\keyword{ ts }
