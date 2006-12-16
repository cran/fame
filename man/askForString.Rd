\name{askForString}
\alias{askForString}
\alias{askForPassword}
\title{Ask User for a Password or String}
\description{Prompt the user for a password or a string. These functions
  interact with the user differently depending on the environment in
  which \R is running.
}
\usage{
askForString(prompt = "?: ", default = "")
askForPassword(prompt = "Password: ")
}
\arguments{
  \item{prompt}{Prompt to show when asking for user input}
  \item{default}{String shown as initial answer in dialog box or Emacs
	minibuffer, if either is available}
}
\note{If \R is running under Emacs, the password prompt is shown in
  the minibuffer rather than in a dialog box in order to take advantage
  of the \code{read-passwd} function in Emacs, which does shows
  astericks as the password is entered.
}
\value{The password or string entered.}
\author{ Jeff Hallman }
#ifdef windows
\seealso{ \code{\link{readline}}, \code{\link{winDialogString}}}
#endif
#ifdef unix
\seealso{ \code{\link{readline}}}
#endif
\keyword{utilities}
