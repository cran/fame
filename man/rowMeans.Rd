\name{RowMeans}
\alias{RowSums}
\alias{RowSums.default}
\alias{RowSums.tis}
\alias{RowMeans}
\alias{RowMeans.default}
\alias{RowMeans.tis}
\title{Form Row Sums and Means}
\description{
  Form row sums and means for numeric arrays.
}
\usage{
RowSums (x, \dots)
RowMeans(x, \dots)
\method{RowSums}{default}(x, \dots)
\method{RowMeans}{default}(x, \dots)
\method{RowSums}{tis}(x, \dots)
\method{RowMeans}{tis}(x, \dots)
}
\arguments{
  \item{x}{an array of two or more dimensions, containing numeric,
    complex, integer or logical values, or a numeric data frame, or a
    \code{tis} time indexed series}
  \item{\dots}{arguments passed along to \code{rowSums} or \code{rowMeans}.}
}
\value{
  The \code{tis}-specific methods return a \code{tis}.

  For other types of \code{x}, see \code{\link{rowMeans}} or \code{\link{rowSums}}.
}
\seealso{\code{\link{rowMeans}}, and \code{\link{rowSums}}}
\examples{
mat <- tis(matrix(1:36, ncol = 3), start = latestJanuary())
cbind(mat, rowSums(mat), rowMeans(mat))
}
\keyword{array}
\keyword{algebra}
\keyword{ts}
