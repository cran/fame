.onLoad <- function(libname, pkgname){
  setDefaultFrequencies(setup = T)
  assign("tzOffsetSeconds",
         as.vector(unclass(ISOdatetime(2007,1,1,0,0,0, "")) - 
                   unclass(ISOdatetime(2007,1,1,0,0,0, "GMT"))),
         env = globalenv())
  addLast(.Last.fame)
}
