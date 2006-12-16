\name{rowMeans}
\alias{rowSums}
\alias{rowSums.default}
\alias{rowSums.tis}
\alias{rowMeans}
\alias{rowMeans.default}
\alias{rowMeans.tis}
\title{Form Row Sums and Means}
\description{
  Form row sums and means for numeric arrays.
}
\usage{
rowSums (x, \dots)
rowMeans(x, \dots)
\method{rowSums}{default}(x, na.rm = FALSE, dims = 1, \dots)
\method{rowMeans}{default}(x, na.rm = FALSE, dims = 1, \dots)
\method{rowSums}{tis}(x, \dots)
\method{rowMeans}{tis}(x, \dots)
}
\arguments{
  \item{x}{an array of two or more dimensions, containing numeric,
    complex, integer or logical values, or a numeric data frame, or a
    \code{tis} time indexed series}
  \item{\dots}{arguments passed along to \code{rowSums.default} or
	\code{rowMeans.default}, which are actually the versions of
	\code{rowSums} and \code{rowMeans} from the \code{base}
	package. The \dots argument is ignored in \code{rowSums.default} and
	\code{rowMeans.default}.}
  \item{na.rm}{logical. Should missing values (including \code{NaN})
    be omitted from the calculations?}
  \item{dims}{Which dimensions are regarded as \dQuote{rows} or
    \dQuote{columns} to sum over.  For \code{row*}, the sum or mean is
    over dimensions \code{dims+1, \dots}; for \code{col*} it is over
    dimensions \code{1:dims}.}
}
\details{
  These functions are equivalent to use of \code{\link{apply}} with
  \code{FUN = mean} or \code{FUN = sum} with appropriate margins, but
  are a lot faster.  As they are written for speed, they blur over some
  of the subtleties of \code{NaN} and \code{NA}.
  If \code{na.rm = FALSE} and either \code{NaN} or \code{NA} appears in
  a sum, the result will be one of \code{NaN} or \code{NA}, but which
  might be platform-dependent. 
}
\value{
  A numeric or complex array of suitable size, or a vector if the result is
  one-dimensional.  The \code{dimnames} (or \code{names} for a vector
  result) are taken from the original array.

  If there are no values in a range to be summed over (after removing
  missing values with \code{na.rm = TRUE}), that
  component of the output is set to \code{0} (\code{rowSums}) or \code{NA}
  (\code{rowMeans}), consistent with \code{\link{sum}} and
  \code{\link{mean}}.

  The \code{tis}-specific methods also return a \code{tis}.
}
\seealso{
  \code{\link{apply}}, \code{\link{rowsum}}, and \code{\link{colSums}}
  for more details and examples.
}
\examples{
mat <- tis(matrix(1:36, ncol = 3), start = latestJanuary())
cbind(mat, rowSums(mat), rowMeans(mat))
}
\keyword{array}
\keyword{algebra}
\keyword{ts}
