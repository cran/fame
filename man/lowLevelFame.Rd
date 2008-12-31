\name{lowLevelFame}
\alias{fameRunning}
\alias{fameStart}
\alias{fameStop}
\alias{fameState}
\alias{fameCommand}
\alias{fameStatusMessage}
\alias{fameDbOpen}
\alias{fameDbClose}
\alias{fameDeleteObject}
\alias{fameWriteSeries}
\alias{fameWhat}
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

  \code{fameStart} initializes the FAME HLI and may open a work database.
  Since the work database is always the first one opened, its key is
  always 0.

  \code{fameStop} kills the HLI session and the FAME SERVER process
  started by \code{fameStart}.  In any given R session, you
  cannot restart the HLI once it has died for any reason. (This is a
  FAME limitation, not an \R one.)  Death of the R process also kills
  the child FAME SERVER process.  So it rarely makes sense to call
  \code{fameStop} explicitly, as it makes any subsequent FAME
  interaction in the current R session impossible.

  \code{fameState} returns the state of the FAME HLI interface, which is
  one of \code{c("none", "starting", "running", "dead")}.  "none" means
  the HLI has not been started, "starting" means the HLI has not
  finished starting yet (this usually indicates an error), "running"
  means the HLI is up and running, while "dead" means the HLI has been
  killed, either by an error or by \code{fameStop}.  In either case,
  once the HLI is "dead", it cannot be revived.

  \code{fameCommand} sends its string argument to the child FAME SERVER
  process to be executed.  If \code{silent} is \code{TRUE}, it invisibly
  returns a status code that can be sent to \code{fameStatusMessage} to
  get an error message.  If \code{silent} is \code{FALSE}, the status
  message is echoed to standard output.  If \code{capture} is
  \code{TRUE}, output from the FAME output channel is returned, with a
  "status" attribute holding the FAME status code.

  \code{fameStatusMessage} looks up and returns the error message
  associated with its argument.

  \code{fameDbOpen} opens the named database in the given access mode.
  It returns an integer \code{dbKey}, which is a required argument for
  some of the other functions documented here.

  \code{fameDbClose} closes the database associated with the
  given \code{dbKey}.

  \code{fameDeleteObject} deletes a named object from a database.

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
}  
\usage{
fameRunning()
fameStart(workingDB = TRUE)
fameStop()
fameState()
fameCommand(string, silent = T, capture = F)
fameStatusMessage(code)
fameDbOpen(dbName, accessMode = "read")
fameDbClose(dbKey)
fameDeleteObject(db, fname)
fameWriteSeries(dbKey, fname, ser, update = F, checkBasisAndObserved = F)
fameWhat(dbKey, fname, getDoc = F)
}
\arguments{
  \item{workingDB}{if \code{TRUE} (the default) open a temporary working
  database whose \code{dbKey} will be 0}
  \item{string}{a FAME command to be executed}
  \item{silent}{run the command quietly if \code{TRUE}}
  \item{capture}{capture and return strings from the FAME output channel
	if \code{TRUE}}
  \item{code}{an integer status code from FAME}
  \item{dbName}{name of or path to the database to open}
  \item{accessMode}{a string specifying the access model to open the
	database in: one of "read", "create", "overwrite", "update", or
	"shared".} 
  \item{dbKey}{integer returned by \code{dbOpen}}
  \item{db}{can take \code{dbKey} or \code{dbName} form; that is, it can
	be an integer returned by \code{dbOpen}, or it can be the name of a
	database or path to a database.}
  \item{fname}{name of an object in a FAME database}
  \item{ser}{a \code{tis} time series}
  \item{update}{if \code{TRUE} update any existing series by the same
	name in place.  If \code{FALSE}, replace existing series.}
  \item{checkBasisAndObserved}{ see description above for \code{fameWriteSeries}}
  \item{getDoc}{if \code{TRUE}, also return the \code{description} and
  \code{documentation} attributes.}
}
\value{
  \code{fameRunning} return a Boolean.

  \code{fameStart} and \code{fameStop} return nothing.

  \code{fameStatus} returns a string as documented above.

  If \code{capture} is \code{FALSE}, \code{fameCommand} invisibly returns a status code.
  If \code{capture} is \code{TRUE}, strings sent to the FAME output
  channel are returned as a character vector, and the status code is
  returned as the "status" attribute of that vector.
  
  \code{fameStatusMessage} returns a message string.

  \code{fameDbOpen} returns an integer \code{dbKey}.

  \code{fameDbClose} returns a status code.

  \code{fameDeleteObject}  returns a status code.

  \code{fameWriteSeries}  returns a status code.

  \code{fameWhat} returns a list.
}
\seealso{ \code{\link{getfame}}, \code{\link{putfame}}, \code{\link{fameCustomization}}}
\keyword{ database }
