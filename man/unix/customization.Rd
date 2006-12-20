\name{fameCustomization}
\alias{fameLocalPath}
\alias{fameLocalInit}
\title{Local Customization of the Fame Interface}
\description{
  The first time one of the functions that interfaces with a Fame
  database is called, the internal function \code{fameStart} is called
  to initialize the HLI (Host Language Interface) and open a work
  database. After accomplishing that, \code{fameStart} checks to see if
  a function named \code{fameLocalInit} exists.  If so, it is called
  with no arguments immediately after opening the work database.

  At the Federal Reserve Board, we have some code in .pc files that we
  always load when running Fame.  To do this, we define:
  
  \preformatted{
    fameLocalInit <- function(){ 
      fameCommand('load file("/opt/fame/frb/pc/lib.pc")', silent = T) 
      fameCommand('load file("/opt/fame/frb/pc/syslib.pc")', silent = T)
    }
  }

  \code{getfame}, \code{putfame}, \code{fameWhats} and
  \code{fameWildlist} all take a string \code{db} argument to specify
  the database to open. The string is fed to the internal function
  \code{getFamePath} to find the database. \code{getFamePath(dbString)}
  first checks to see if its argument is already the path to an existing
  file.  If so, it is returned.  If not, and a local function 
  called \code{fameLocalPath} exists, \code{dbString} is replaced by
  \code{fameLocalPath(dbString)} and the check is made again.  If the
  result is a valid path to an existing file, it is returned.
  Otherwise, \code{getFamePath(dbString)} returns \code{NULL}.

  At the Federal Reserve Board, some of our FAME databases are "registered", i.e.,
  there is a shell script maintained by the database administration
  staff that takes a short name (like "us" or "intl") and returns a path
  name.  Our local \code{fameLocalPath} calls that script and returns
  the full path name to the database, or the original string if the
  database was not found.
}  
\author{ Jeff Hallman }
\keyword{ database }
