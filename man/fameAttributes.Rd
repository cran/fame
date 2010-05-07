\name{fameAttributes}
\alias{fameAttribute}
\alias{fameSetAttribute}
\alias{fameCreated}
\alias{fameUpdated}
\title{Fame Attributes}
\description{
  FAME has some predefined attributes (such as creation and update dates) for
  databases and data objects, and also allows user defined attributes for
  objects in a database. Currently, this package deals only with data object
  attributes. 

  Among the more useful predefined attributes you can access are an objects
  creation and update dates, via the \code{fameCreated} and \code{fameUpdated}
  convenience functions. However, you can also use the more general
  \code{fameAttribute} function to access a string representation of any
  attribute, including user-defined ones.

  Setting attributes

  Before user-defined attributes can be set for a data object, the database itself must
  know the names and types of the attributes. Each data object can have up to
  twenty user-defined attributes of each of six data types: boolean, date,
  namelist, numeric, precision, and string. The names of the user-defined
  attributes are stored in the database in six namelist scalars named
  boolean_attribute_names, date_attribute_names, and so on, one for each data type.
  Functions will be provided shortly to do this.
  
  Once the database knows what attributes are allowed, they can be set
  via \code{fameSetAttribute}.
  
  \code{fameAttribute} reads one attribute of one object in a Fame
  database and returns it as a string.
  \code{fameSetAttribute} sets the value of one attribute of one object
  in a Fame database.
  \code{getFameCreated} and \code{getFameUpdated} return an object's
  \code{created} and \code{updated} attributes, respectively, as
  \code{POSIXlt} dates.
}
\usage{
fameAttribute(attribute, fname, db)
fameSetAttribute(attribute, value, fname, db)
fameCreated(fname, db)
fameUpdated(fname, db)
}
\arguments{
  \item{attribute}{
	string naming the attribute to be returned}
  \item{fname}{
    name of an object in a FAME database}
  \item{db}{
    string giving the relative or full path to the Fame database to read
    or write from. If the \code{fameLocalPath()} function is defined, it
    will be called on this argument to obtain a path.}
  \item{value}{
    value to set the attribute to}
}
\value{
  \code{fameAttribute} returns a named string.
  
  \code{fameSetAttribute} returns whatever strings FAME wrote to
  standard output while setting the attribute.

  \code{fameCreated} and \code{fameUpdated} each return a
  \code{POSIXlt} object.
}
\examples{
\dontrun{
fameUpdated("gdp.q", db = "somedb")
}
}
\keyword{ database }
