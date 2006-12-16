.onLoad <- function(libname, pkgname){
  setDefaultFrequencies(setup = T)
  addLast(.Last.fame)
  ## this stuff needed to make Fame work
  ## just why Fame needs stuff from the Motif/LessTif library to work when
  ## there's no user interface involved is a question that will undoubtedly be
  ## pondered for the ages.
  libPath <- Sys.getenv("LD_LIBRARY_PATH")
  XmLib <- "/usr/X11R6/lib/libXm.so"
  linkDir <- tempdir()
  tmpLink <- file.path(linkDir, "libXm.so.1")
  system(paste("ln -s", XmLib, tmpLink))
  Sys.putenv("LD_LIBRARY_PATH" = paste(libPath, linkDir, sep = ":"))
}
