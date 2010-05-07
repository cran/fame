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

  \code{fameLocalPath}: Used to customize \code{\link{getFamePath}}. 
}  
\keyword{ database }
