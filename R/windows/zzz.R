.onLoad <- function(libname, pkgname){
  setDefaultFrequencies(setup = T)
  addLast(.Last.fame)
  ## library.dynam("fame", package = "fame")
}
