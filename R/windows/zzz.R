.onLoad <- function(libname, pkgname){
  ## Could not figure out how to make Windows dynamic linking work, so instead
  ## load the chli.dll first, then the fame.dll after.

  ## find chli.dll and load it
  fameDir <- Sys.getenv("FAME")
  if(nchar(fameDir) == 0){
    defaultDir <- "C:/Program Files/FAME"
    cat(paste("FAME environment variable not set; assuming", defaultDir))
    fameDir <- defaultDir
    fameDir <- sub("/$", "", chartr("\\", "/", fameDir))
  }
  
  if(!file.exists(fameDir)){
    noFameMessage <-
      paste("FAME directory", fameDir, "not found.\n",
            "If you have FAME installed, specify it's location via the FAME environment variable.\n",
            "Otherwise, this package is pretty useless.\n")
    cat(noFameMessage)
  }
  else {
    chliPath <- file.path(fameDir, "chli.dll")
    if(!file.exists(chliPath))  stop(paste("chli.dll not found in", fameDir))
    
    dyn.load(chliPath)
    if(!is.loaded("cfmini"))
      stop("Could not load chli.dll")
  }
  ## find and load fame.dll 
  libsDir <- file.path(libname, pkgname, "libs")
  dllPath <- file.path(libsDir, paste(pkgname, "dll", sep = "."))
  if(!file.exists(dllPath)) stop(paste("fame.dll not found in", libsDir))
  dyn.load(dllPath)
}
