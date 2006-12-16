\name{hexidecimal}
\alias{hexidecimal}
\alias{hex2numeric}
\title{Hexidecimal conversions}
\description{Convert numeric vectors to hexidecimal strings and vice versa.}
\usage{
hexidecimal(dec)
hex2numeric(hex)
}
\arguments{
  \item{dec}{numeric vector}
  \item{hex}{character vector of hexidecimal strings}
}
\details{Hexidecimals are base 16 numbers with digits represented by the
  characters '0123456789abcdef'.  \code{hex2numeric("1df")}, for
  example, is 479 (256 + 13*16 + 15).

  Hex numbers are often used to represent bits in a byte, since '9F'
  takes up less space than '10011111'.  TCP/IP port numbers are also
  often represented in hex.
}
\value{\code{hexidecimal} returns a character object like \code{dec}.

  \code{hex2numeric} returns a numeric object like \code{hex}.
}
\author{ Jeff Hallman }
\keyword{ utilities }
