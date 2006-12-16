\name{sendSocketObject}
\alias{sendSocketObject}
\alias{receiveSocketObject}
\title{Send and Receive \R Objects through Sockets }
\description{
  These functions allow \R sessions on different machines to send \R
  objects to each other.  Consider two \R sessions running on (possibly)
  different machines.  Call the sessions R1 and R2, with R1 running on
  somehost.somewhere.com.  To send an object from R1 to R2, the sequence
  of events goes like this: 

  1.  R1 invokes \code{receiveSocketObject(NNNNN)}, where NNNNN is an
      available port number on R1's localhost. (Use
      \code{\link{availablePort}} to find a port number.) The R1
      session hangs until:

  2.  R2 invokes \code{sendSocketObject(object, "somehost.somewhere.com", NNNNN)}
      to send \code{object} to R1.  If R1 is not listening on port NNNNN
      already, this operation will hang R2 until R1 invokes
      \code{receiveSocketObject(NNNNN)} to start listening there.

  3.  R1 and R2 both return after \code{object} has been sent, with
      \code{receiveSocketObject} on R1 returning the object that was sent.
}
\usage{
sendSocketObject(object, host, port)
receiveSocketObject(port)
}
\arguments{
  \item{object}{an \R object}
  \item{host}{DNS name of the host to which \code{object} is being sent}
  \item{port}{TCP port number on the receiving end}
}
\details{
  \code{sendSocketObject} opens a blocking binary client socket
  connection to the given \code{host} and \code{port}, serializes
  \code{object} onto the connection, then closes it.

  \code{receiveSocketObject} listens on a blocking binary server socket
  and returns the \R object sent over it.
}
\value{
  \code{receiveSocketObject()} returns the object sent.
}
\author{Jeff Hallman}
\note{\code{serialize} puts \code{object} on the socketConnection, while
  code{unserialize} reads it at the other end.}
\seealso{\code{\link{availablePort}} to find a port number, and
  \code{startRemoteServer} to run a server that accepts \R commands
  over a socket and returns the resulting \R objects}
\keyword{utilities}
