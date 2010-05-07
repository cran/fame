\name{fameAddAttribute}
\alias{fameAddAttribute}
\title{Add a FAME Attribute}
\description{Adds a user-defined attribute to data items in a FAME database. Data
  items in a FAME database can have up to 20 user-defined attributes of
  each of 6 types. Before you can set an item's \code{blah} attribute,
  the database itself must know about the attribute. This function
  defines the attribute to the database, so that \code{fameSetAttribute}
  can set it. 
}
\usage{
fameAddAttribute(name, type = c("string", "date", "boolean", "precision", "numeric", "namelist"), db)
}
\arguments{
  \item{name}{name of the attribute}
  \item{type}{one of the six predefined types. Note that FAME
  "precision" series are 64-bit doubles, while FAME "numeric" items are
  32-bit floats. 
}
  \item{db}{
    string giving the relative or full path to the Fame database to read
    or write from. If the \code{fameLocalPath()} function is defined, it
    will be called on this argument to obtain a path.}
}
\value{invisibly returns a character vector of whatever FAME wrote to
  standard output or standard error while attempting the assignment.
}
\seealso{
\code{\link{fameSetAttribute}} to set attributes once they've been defined.
}
\keyword{ database }
