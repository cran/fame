\name{lowLevelFame}
\alias{fameRunning}
\alias{fameStart}
\alias{fameStop}
\alias{fameCommand}
\alias{fameStatusMessage}
\alias{fameDbOpen}
\alias{fameDbClose}
\alias{fameDeleteObject}
\alias{fameWriteSeries}
\alias{fameWhat}
\alias{getFamePath}
\alias{fameLocalInit}
\title{Low Level Fame Interface Functions}
\description{
  These are most of the lower level functions used in the
  FAME interface. Most users will never need any of these functions,
  as the higher level function \code{getfame} and \code{putfame} do
  almost everything they want to do.  The functions documented here were
  written in the course of implementing \code{getfame} and
  \code{putfame}, and some of them may prove useful on their own.

  \code{fameRunning} answers \code{TRUE} if there is a process called
  "FAME SERVER" already running under the user's id and with the current
  R process as its parent process.

  \code{fameStart} initializes the FAME HLI and opens a work database.
  Since the work database is always the first one opened, its key is
  always 0.

  \code{fameStop} kills the HLI session and the FAME SERVER process
  started by \code{fameStart}.  In any given R session, you
  cannot restart the HLI once it has died for any reason. (This is a
  FAME limitation, not an \R one.)  Death of the R process also kills
  the child FAME SERVER process.  So it rarely makes sense to call
  \code{fameStop} explicitly, as it makes any subsequent FAME
  interaction in the current R session impossible.

  \code{fameCommand} sends its string argument to the child FAME SERVER
  process to be executed.  If \code{silent} is \code{TRUE}, it invisibly
  returns a status code that can be sent to \code{fameStatusMessage} to
  get an error message.  If \code{silent} is \code{FALSE}, the status
  message is echoed to standard output.

  \code{fameStatusMessage} looks up and returns the error message
  associated with its argument.

  \code{fameDbOpen} opens the named database in the given access mode.
  It returns an integer \code{dbKey}, which is a required argument for
  some of the other functions documented here.

  \code{fameDbClose} closes the database associated with the
  given \code{dbKey}.

  \code{fameDeleteObject} deletes the named object from the database
  associated with the give \code{dbKey}.

  \code{fameWriteSeries} writes the \code{tis} (Time Indexed Series)
  object \code{ser} as \code{fname} in the database associated with
  \code{dbKey}. If an object by that name already exists in the database
  and \code{update} is \code{TRUE}, the frequency and type of \code{ser}
  are checked for consistency with the existing object, and if
  \code{checkBasisAndObserved} is \code{TRUE} (not the default), those
  items are also checked.  Any inconsistencies cause the update to fail.
  If all checks are OK, then the range covered by \code{ser} is written
  to the database.  If \code{update} if \code{FALSE}, any existing
  series called \code{fname} in the database will be replaced by
  \code{ser}.  This function should probably not be called directly, as
  \code{putfame} provides a nicer interface.

  \code{fameWhat} returns a list of low level information about an
  object in a database, including components named status, dbKey, name,
  class, type, freq, basis, observ, fyear, fprd, lyear, lprd, obs, and
  range.  If \code{getDoc} is \code{TRUE}, it will also include
  \code{description} and \code{documentation} components. See the FAME
  documentation for the CHLI functions \code{cfmwhat} and \code{cfmsrng}
  for details.

  \code{getFamePath} finds a path name to a database matching its
  argument.  The default implementation returns the argument if it is a
  valid path to an existing file, or NULL if it is not. You should
  create a local implementation of this function if you have some other
  way to find a valid path to a database.  For example, at the Federal
  Reserve Board, some of our FAME databases are "registered", i.e.,
  their paths can be looked up via a locally written shell script.  Our
  local \code{getFamePath} calls that script and returns the full path
  name to the database, or NULL if something isn't right.

  \code{fameLocalInit} is a function that is called by \code{fameStart}
  immediately after opening the work database.  The default
  implementation does nothing.  You can create a local function with the
  same name that does additional initialization.
}  
\usage{
fameRunning()
fameStart()
fameStop()
fameCommand(string, silent = F)
fameStatusMessage(code)
fameDbOpen(dbName, accessMode = "read")
fameDbClose(dbKey)
fameDeleteObject(dbKey, fname)
fameWriteSeries(dbKey, fname, ser, update = F, checkBasisAndObserved = F)
fameWhat(dbKey, fname, getDoc = F)
getFamePath(dbString)
fameLocalInit()
}
\arguments{
  \item{string}{a FAME command to be executed}
  \item{silent}{run the command quietly if \code{TRUE}}
  \item{code}{an integer status code from FAME}
  \item{dbName}{name of or path to the database to open}
  \item{accessMode}{a string specifying the access model to open the
	database in: one of "read", "create", "overwrite", "update", or
	"shared".} 
  \item{dbKey}{integer returned by \code{dbOpen}}
  \item{fname}{name of an object in a FAME database}
  \item{ser}{a \code{tis} time series}
  \item{update}{if \code{TRUE} update any existing series by the same
	name in place.  If \code{FALSE}, replace existing series.}
  \item{checkBasisAndObserved}{ see description above for \code{fameWriteSeries}}
  \item{getDoc}{if \code{TRUE}, also return the \code{description} and
  \code{documentation} attributes.}
  \item{dbString}{an existing path name or the name of a registered database.}
}
\value{
  \code{fameRunning} return a Boolean.

  \code{fameStart} and \code{fameStop} return nothing.

  \code{fameCommand} invisibly returns a status code.

  \code{fameStatusMessage} returns a message string.

  \code{fameDbOpen} returns an integer \code{dbKey}.

  \code{fameDbClose} returns a status code.

  \code{fameDeleteObject}  returns a status code.

  \code{fameWriteSeries}  returns a status code.

  \code{fameWhat} returns a list.

  \code{getFamePath} returns the path to a database or NULL if no match
  was found.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{getfame}}, \code{\link{putfame}}}
\keyword{ database }
