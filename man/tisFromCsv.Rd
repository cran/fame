\name{tisFromCsv}
\alias{tisFromCsv}
\title{Read time series from Comma Separated Values (.csv) file}
\description{Reads \code{tis} (Time Indexed Series) from a csv file,
  returning the series in a list, and optionally storing them in an environment.}
\usage{
tisFromCsv(csvFile, dateCol = "date8", dateFormat = "\%Y\%m\%d", defaultTif = "business",
           save = F, envir = parent.frame(),
           naNumber = NULL, tolerance = sqrt(.Machine$double.eps), ...)
}
\arguments{
  \item{csvFile}{A file name, connection, or URL acceptable to
    \code{\link{read.csv}}. Also see the the rest of this help entry for 
    required attributes of this file.}
  \item{dateCol}{name of the column holding dates.  This column must be
	present in the file.} 
  \item{dateFormat}{format of the dates in \code{dateCol}. See
	\code{\link{strftime}} for date formats.}
  \item{defaultTif}{If the frequency can't be inferred from the dates in
    the \code{ymdCol} column, this \code{tif} frequency will be used.
    This should be a rare occurrence.}
  \item{save}{If true, save the individual series in the enviroment
    given by the \code{envir} argument. Default is \code{FALSE}.}
  \item{envir}{if \code{save == TRUE}, the individual series (one per
    column) are saved in this enviroment.  Default is the frame of the caller.}
  \item{naNumber}{if non-\code{NULL}, numbers within \code{tolerance} of
    this number are considered to be \code{NA} values.  NA strings can
    be specified by including an \code{na.strings} argument as one of
    the \dots arguments that are passed along to \code{read.csv}.}
  \item{tolerance}{Used to determine whether or not numbers in the file
    are close enough to \code{naNumber} to be regarded as equal to it.
    The default is about 1.48e-08.}
  \item{\dots}{Additional arguments passed along to the underlying
    \code{read.csv} function.}
}
\details{
  \bold{File Requirements:} The csv file must have column names
  across the top, and everything but the first row should be numeric.
  There must be as many column names (enclosed in quotes) as there are
  columns, and the column named by \code{dateCol} must have dates in the
  format indicated by \code{dateFormat}. \strong{The \code{dateCol}
  column must be present.} 

  \bold{Missing (NA) values:}  Missing and NA values are the same thing.
  The underlying \code{read.csv} has "," as its default separator and
  "NA" as its default na.string, so the rows 
\preformatted{
    
  20051231,,13,,42,NA,
  20060131,NA,14,,43,,NA

}  
  indicate \code{NA} values for both the Dec 2005 and Jan 2006
  observations of the first, third, fifth and sixth series. 

  The values in the file are read into a single large \code{tis} series,
  with a \code{tif} (Time Index Frequency) inferred from the first six
  dates in the ymd column.  The first date is converted to a \code{ti}
  (Time Index) of that frequency and becomes the \code{start} of the
  series.  Each individual column is then windowed via \code{naWindow}
  to strip off leading and trailing \code{NA} values, and the resulting
  series are put into a list with names given by lower-casing the column
  names from the csv file.  If \code{save} is \code{TRUE}, the series
  are also stored in \code{envir} using those same names.
}
\value{A list of \code{tis} time series, one per column of the csv file.
  The list is returned invisibly if \code{save} is \code{TRUE}.
}
\author{Jeff Hallman}
\seealso{ \code{\link{ti}}, \code{\link{tis}}, \code{\link{read.csv}},
  \code{\link{read.table}}} 
\keyword{file}
\keyword{connection}
