getfame <- function(sernames, db, save = F, envir = parent.frame(),
                    start = NULL, end = NULL, getDoc = T){
  partialCall <- bquote(getfame(sernames = .(sernames),
                                db = .(db),
                                start = .(start),
                                end = .(end),
                                getDoc = .(getDoc)))
  ensureValidServer()
  sendToServer(partialCall)
  zz <- receiveFromServer()
  if(save){
    for(name in names(zz)){
      assign(name, zz[[name]], env = envir)
    }
    invisible(zz)
  }
  else return(zz)
}

putfame <- function(serlist, db,
                    access = "shared",
                    update = T,
                    checkBasisAndObserved = F,
                    envir = parent.frame()){
  ## create z, a list of univariate tis series to be written to Fame
  if(is.character(serlist)){
    zz <- vector("list", length(serlist))
    if(is.null(names(serlist))) names(zz) <- serlist
    else                        names(zz) <- names(serlist)
    for(i in 1:length(serlist)){
      rname <- serlist[i]
      if(exists(rname, envir = envir))
        obj <- get(rname, envir = envir)
      else stop(paste(rname, "not found"))
      if(!isTis(obj))
        stop(paste(rname, "is not a tis series"))
      zz[[i]] <- obj
    }
  }
  else {
    if(is.list(serlist))
      zz <- serlist
    else {
      zz <- list(serlist)
      names(zz) <- deparse(substitute(serlist))
    }
  }
  if(!all(sapply(zz, isTis)))
    stop("non-tis argument")

  nser <- length(zz)
  if(is.null(fameNames <- names(zz)))
    fameNames <- character(nser)
  
  nz <- sum(sapply(zz, NCOL))
  z <- vector("list", nz)
  znames <- character(nz)
  i.z <- 0
  for(i.sl in seq(zz)){
    ser <- zz[[i.sl]]
    sercols <- NCOL(ser)

    i.z <- max(i.z) + 1:sercols

    if(is.matrix(ser)){
      if(length(colnames(ser)) == sercols)
		znames[i.z] <- colnames(ser)
      else if(sercols == 1) znames[i.z] <- fameNames[i.sl]

      for(i in 1:sercols) z[[i.z[i]]] <- ser[,i]
    }
    else {
      znames[i.z] <- fameNames[i.sl]
      z[[i.z]] <- ser
    }
    if(any(nchar(znames[i.z]) == 0))
      stop("unnamed series")
  }
  names(z) <- znames
  ## z is finally ready now
  partialCall <-
    bquote(putfame(serlist = .(z),
                   db = .(db),
                   update = .(update),
                   checkBasisAndObserved = .(checkBasisAndObserved)))
  pcList <- as.list(partialCall)
  if(!missing(access)) pcList$access <- access
  partialCall <- as.call(pcList)
  ensureValidServer()
  sendToServer(partialCall)
  return(receiveFromServer())
}

fameWhats <- function(db, fname, getDoc = T){
  partialCall <- bquote(fameWhats(db = .(db),
                                  fname = .(fname),
                                  getDoc = .(getDoc)))
  ensureValidServer()
  sendToServer(partialCall)
  return(receiveFromServer())
}

fameWildlist <- function(db, wildString = "?", nMax = 1000, charMode = T){
  partialCall <- bquote(fameWildlist(db = .(db),
                                     wildString = .(wildString),
                                     nMax = .(nMax),
                                     charMode = .(charMode)))
  ensureValidServer()
  sendToServer(partialCall)
  return(receiveFromServer())
}
