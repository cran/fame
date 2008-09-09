\name{fameCustomization}
\alias{fameCustomization}
\title{Local Customization of the Fame Interface}
\description{
  You can define two local functions, \code{fameLocalInit} and
  \code{fameLocalPath}, to customize some aspects of the FAME interface.
  
  \code{fameLocalInit}:  The first time one of the functions that
  interfaces with a Fame database is called, the internal function
  \code{fameStart} is called to initialize the HLI (Host Language
  Interface) and open a work database. After accomplishing that,
  \code{fameStart} checks to see if a function named
  \code{fameLocalInit} exists.  If so, it is called with no arguments
  immediately after opening the work database. 

  \code{fameLocalPath}: The functions \code{getfame}, \code{putfame},
  \code{fameWhats} and \code{fameWildlist} all take a string \code{db}
  argument to specify the database to open. The string is fed to the
  internal function \code{getFamePath} to find the database. If a
  function called \code{fameLocalPath} exists, \code{dbString} is
  checked against the value returned by \code{fameLocalPath(dbString)}.
  If they are not the same, the latter is returned.  Otherwise, the
  function returns \code{dbString} if it is a valid path to an existing
  readable file, or \code{NULL} if it is not. 
}  
\author{ Jeff Hallman }
\keyword{ database }
