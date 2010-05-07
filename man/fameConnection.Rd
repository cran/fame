\name{fameConnection}
\alias{fameConnection}
\alias{print.fameConnection}
\alias{close.fameConnection}
\title{Fame 'master/dbback' server connection
}
\description{Creates a connection to a Fame service running on a remote host.
}
\usage{
fameConnection(service = "", host = "", user = "", password = "", stopOnFail = TRUE)
\method{print}{fameConnection}(x, \dots)
\method{close}{fameConnection}(con, \dots)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{service}{The name of the service on the server, e.g., "someService"}
  \item{host}{The server name }
  \item{user}{User name on the server}
  \item{password}{A string}
  \item{stopOnFail}{if \code{TRUE} (the default), failing to connect
  generates an R error. Otherwise, the function completes but the result
  is kind of useless.}
  \item{x}{A fameConnection}
  \item{con}{A fameConnection}
  \item{\dots}{For \code{print}, these are arguments passed on to
  \code{print.matrix}. For \code{close}, they're ignored. }
}
\details{\code{fameConnection} uses the C HLI function cfmopen to
  create a connection to a Fame service. The connection it returns can
  be used in subsequent calls to \code{\link{fameWildlist}} or \code{\link{getfame}}.

  The \code{print} and \code{close} functions, when called on a
  fameConnection, do what they say.
}
\value{A fameConnection is an integer with attributes "service", "host",
  "user" and "password".
}
\note{These functions have not really been tested at all, as the package
  maintainer does not have ready access to a Fame server.
}
\seealso{\code{\link{getfame}}, \code{\link{fameWildlist}}}
\keyword{ database }
