\name{getfame}
\alias{getfame}
\alias{putfame}
\alias{fameWhats}
\alias{fameWildlist}
\title{ Fame Interface }
\description{
  \code{getfame} and \code{putfame} read and write scalars (\code{ti,
  numeric, character} or \code{logical} objects of length 1) and
  numeric tis (Time Indexed Series) from and to Fame databases.
  FAME case series and series with string or FAME "date" types can be
  read by \code{getfame}, but not written with \code{putfame}. 
  
  \code{fameWhats} returns information about an object in a database,
  including its name, class, type, basis and observed attributes, as
  well as start (a \code{ti} Time Index) and length. If \code{getDoc} is
  \code{TRUE}, it will also include \code{description} and
  \code{documentation} components.

  \code{fameWhats} is a wrapper around the function \code{fameWhat},
  which provides the same information in a lower-level form.

  \code{fameWildlist} returns a list giving the name, class, type and
  frequency of the objects in the database with names that match
  \code{wildString}.

}
\usage{
getfame(sernames, db, connection = NULL, save = FALSE, envir = parent.frame(),
        start = NULL, end = NULL, getDoc = TRUE)
putfame(serlist, db, access = "shared", update = TRUE,
        checkBasisAndObserved = FALSE, envir = parent.frame()) 
fameWhats(db, fname, connection = NULL, getDoc = TRUE)
fameWildlist(db, wildString = "?", connection = NULL, nMax = 1000, charMode = TRUE)
}
\arguments{
  \item{sernames}{
    character vector of Fame names of series and/or scalars to retrieve. 
  }
  \item{db}{
    string giving the relative or full path to the Fame database to read
    or write from. If the \code{fameLocalPath()} function is defined, it
    will be called on this argument to obtain a path.  To open a remote
    database, use a string like "servername /path/to/a/database.db".
  }
  \item{connection}{
    created by calling \code{\link{fameConnection}}. Leave this at the
    default \code{NULL} setting for local databases.
  }
  \item{save}{
    if \code{TRUE} the retrieved series are individually saved in the
    environment specified by \code{envir}.
  }
  \item{envir}{
    for \code{getfame}, the environment used by \code{assign} to save
    the retrieved series if \code{save} is \code{TRUE}.  For
    \code{putfame}, if \code{serlist} is a character vector, the
    environment in which to find the series that will be stored in the
    database.  The default environment for both functions is the frame
    of the caller.
  }
  \item{start}{
    a \code{ti} object, or something that the \code{\link[tis:ti]{ti}} function
    can turn into a \code{ti} object.  The time index for the first
    observation in the returned series.  The default is the series start
    in the database.
  }
  \item{end}{
    a \code{ti} object, or something that the \code{\link[tis:ti]{ti}} function
    can turn into a \code{ti} object.  The time index for the last
    observation in the returned series.  The default is the series end
    in the database.
  }
  \item{getDoc}{
    if \code{TRUE} (the default), also get the series \code{description} and
    \code{documentation} attributes, accessible via functions of the
    same names.
  }
  \item{serlist}{
    the objects to be written to the database.  This can be a character
    vector giving the names of the objects in the environment specified
    by \code{envir}, a list containing the objects, or an object itself.
    Objects that can be written to the database include scalars (single
    numbers, strings, and \code{ti} dates), and \code{tis} series. 
  }
  \item{access}{
    string specifying the access mode to open the database in.  Should
    be one of \code{c("create", "overwrite", "read", "shared",
    "update")}. \bold{Warning:} Opening an existing database in
    "overwrite" mode wipes out the database.  You almost never want to
    do this.
  }
  \item{update}{
    if \code{TRUE} (the default), existing series in the database will
    be updated.  If \code{FALSE}, existing series in the database with
    the same names will be replaced by the series in \code{serlist}.
    Fame scalars (single strings, numbers or \code{ti} dates) always
    overwrite their previous incarnations.
  }
  \item{checkBasisAndObserved}{
    if \code{TRUE} and \code{update} == \code{TRUE}, the basis and
    observed attributes of any existing series with the same name will
    be checked for consistency with the updating series from
    \code{serlist}.  If the basis or observed attributes differ, the
    update will not happen.  This argument has no effect with respect to
    non-series objects.
  }
  \item{fname}{
    name of an object in a FAME database}
  \item{wildString}{
    string containing FAME wildcards}
  \item{nMax}{
    maximum number of matches to return}
  \item{charMode}{
    if \code{TRUE} (the default) return \code{class},
    \code{type} and \code{freq}	components as strings, rather than
    integer codes.} 
}
\details{
  Fame names vs. R names:

  The R names of series may differ from their Fame names.  For
  \code{getfame}, \code{names(sernames)} holds the R names of the
  retrieved series.  If \code{sernames} does not have a names
  attributes, the R names will be the same as the Fame names.

  Naming for \code{putfame} is more complicated, because the series
  specified by \code{serlist} for \code{putfame} may be univariate or
  multivariate.  For a multivariate series, the column names of the matrix
  become the Fame names.  Not having a name for each column is thus an
  error.

  A univariate series may be a single-column matrix.  If it is, and it
  has a column name, that becomes the Fame name of the series.
  Otherwise, the Fame name of a univariate series is the corresponding
  element of \code{names(serlist)}.  If \code{serlist} is an actual list
  of series, \code{names(serlist)} must be of the same length.  For
  character vector \code{serlist} a names attribute is optional.  If
  there isn't one, the Fame names will be the same as the R names.

  Consistency checking when \code{update} == \code{TRUE}:

  If there is already an existing series in the database with the same
  name as one in \code{serlist}, the Fame class, type, and frequency
  are checked for consistency between the updating series and the
  existing series in the database.  In addition, if
  \code{checkBasisAndObserved} is \code{TRUE}, those attributes are also
  checked.  Inconsistencies for any of the checked attributes between
  the updating existing series will abort the update.  The default value
  for \code{checkBasisAndObserved} is set to \code{FALSE} because this
  inconsistency is very common in MRA code. 
}
\value{
  \code{getfame} returns a list of the retrieved objects.  If \code{save}
  is \code{T}, the list is returned invisibly.  The names of the list
  are the R names described in the details.  Fame scalars are returned
  as strings, \code{ti} dates, or numbers.  Case series are returned as
  vectors of the appropriate type, with the names attribute of each
  vector set to the case numbers.  If \code{getDoc}
  is \code{TRUE} (the default), retrieved objects will also have
  attributes named \code{description} and \code{documentation}.

  \code{putfame} invisibly returns an empty string.
}
\note{
  These functions use the Fame HLI and a child server
  process. The function \code{fameRunning} is called to see if the
  server process is already running.  If not, \code{fameStart} starts it
  and the HLI.  Your .Last function should call \code{fameStop} to shut
  them down gracefully.  In any given R session, once the Fame HLI has
  died for any reason, it cannot be restarted.  This is a Fame
  limitation.  On exit, \code{getfame} always closes whatever databases
  it opened, so there's no reason not to just leave the server running
  as long as the R session is alive.  Death of the R process kills the
  server process as well.

  Please note that the \code{fameConnection} functionality is not well-tested.
}
\seealso{ \code{\link[tis:ti]{ti}}, \code{\link[tis:tis]{tis}}, \code{\link{fameConnection}}}
\examples{
\dontrun{
mydb <- "pathToMyDatabase"
boink <- getfame("gdp.q", db = mydb)     ## returns a list
gpd.q <- boink[[1]]                      ## or boink$gdp.q
getfame("gdp.q", db = mydb, save = TRUE) ## saves gdp.q in the current frame

## saves the series as "nominalIncome"
getfame(c(nominalIncome = "gdp.q"), db = mydb, save = TRUE)
seriesA <- tis(1:24, start = c(2002, 1), freq = 12)
seriesB <- tis(1:104, start = c(2002, 1), tif = "wmonday")
documentation(seriesB) <- paste("Line", 1:4, "of seriesB documentation")
## store them as "mser" and "wser"
putfame(c(mser = "seriesA", wser = "seriesB"), db = "myfame.db")

matrixSeries <- cbind(a = seriesA, b = seriesA + 3)
putfame(matrixSeries, db = "myfame.db") ## stores as "a" and "b" in Fame

## storing a scalar as "myscalar"
putfame(c(myscalar = 42), db = "myfame.db") 

fameWildlist("myfame.db")
fameWhats("myfame.db", fname = "wser", getDoc = TRUE)
}
}
\keyword{ ts }
\keyword{ database }
