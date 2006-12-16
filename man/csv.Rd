\name{csv}
\alias{csv}
\title{ Writes a CSV (comma separated values) file.}
\description{
  Write a matrix or Time Indexed Series to a .csv file that can be
  imported into a spreadsheet.
}
\usage{
csv(z, file = "", noDates = F, row.names = !is.tis(z), ...)
}
\arguments{
  \item{z}{
	matrix or \code{\link{tis}} object
  }
  \item{file}{
	either a character string naming a file or a connection. If
	\code{""}, a file name is constructed by deparsing \code{z}. The
	extension ".csv" is appended to the file name if it is not already there.
  }
  \item{noDates}{
	logical.  If \code{FALSE} (the default) and \code{z} is a \code{\link{tis}}
	object, the first column of the output file will contain spreadsheet
	dates.
  }
  \item{row.names}{
	either a logical value indicating whether the row names of
    \code{z} are to be written along with \code{z}, or a character vector
    of row names to be written. If \code{FALSE} (the default) and
    \code{z} is a \code{\link{tis}} object, the first column of the
    output file will contain spreadsheet dates.
  }
  \item{\dots}{
	other arguments passed on to \code{write.table}.
  }
}
\details{
  \code{csv} is essentially a convenient way to call \code{write.table}.
  If \code{file} is not a connection, a file name with the ".csv"
  extension is constructed.  Next, a column of spreadsheet dates is
  prepended to \code{z} if necessary, and then \code{csv} calls
  
  \code{write.table(z, file = filename, sep = ",", row.names = !is.tis(z), ...)}

}
\value{
  \code{csv} returns whatever the call to \code{write.table} returned.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{write.table}}}
\keyword{print}
\keyword{file}
