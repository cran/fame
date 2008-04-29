## In a given R session, you cannot restart the Fame HLI once it has died for
## any reason, even if you shut it down gracefully. Death of the R process
## also kills the child Fame SERVER process, and getfame(), at least, always
## closes the databases it opened before it returns.  

fameRunning <- function(){
  ## are we already running a Fame Server process?
  if(runningLinux()) pscmd <- "ps xo user,ppid,command"
  else               pscmd <- "ps -xo user,ppid,command"

  cmd <- paste(pscmd, 
               "| grep", user(),
               "| grep", pid(),
               "| grep -i fame | grep -v defunct | grep -v exiting",
               "| grep -v", shQuote(pscmd))
  
  length(system(cmd, intern = T)) > 0
}

fameStop <- function(){
  if(fameRunning()){
    fameCommand("exit", silent = T)
  status <- .C("cfmfin", status = integer(1), PACKAGE = "fame")$status
  if(status != 0)
    stop(fameStatusMessage(status))
  }
  else warning("No fame process to stop")
}

fameCommand <- function(string, silent = T, capture = F){
  if(capture){
    tfile <- paste(tempfile(), ".txt", sep = "")
    on.exit(file.remove(tfile), add = T)
    fameCommand(paste('output <access overwrite> "',
                      tfile, '"', sep = ""), capture = F)
    status <- fameCommand(string, silent = silent, capture = F)
    fameCommand('output terminal', silent = T, capture = F)
    strings <- readLines(tfile)
    attr(strings, "status") <- status
    return(strings)
  }
  capture.output(boink <- .C("fameCommand", status = integer(1), command = string,
                             errorMsg = character(256), PACKAGE = "fame"))
  if(!silent){
    if(boink$status && string != "exit")
      cat(paste("\nERROR: fameCommand(\"", string, "\") failed\n", sep = ""))
    cat(fameStatusMessage(boink$status), "\n")
    if(boink$status > 512)
      cat(boink$errorMsg, "\n")
  }
  invisible(boink$status)
}

fameStart <- function(){
  ## initialize Fame HLI and open work database. Since the work database is
  ## the first one opened, its key is always 0.
  if(fameRunning()){
    cat("Fame is already running\n")
    return()
  }
  if(!is.loaded("cfmini")){
    if(is.loaded("alarmc")) stop("fame.so built without HLI support")
    else                    stop("fame.so has not been loaded")
  }
  status <- .C("cfmini", status = integer(1), PACKAGE = "fame")$status
  if(status != 0 && status != 1) stop(fameStatusMessage(status))
  boink <- .C("cfmopwk", status = integer(1), key = integer(1), PACKAGE = "fame")
  if(boink$status == 0){
    if(exists("fameLocalInit", mode = "function"))
    fameLocalInit()
  }
  else {
    fameStop()
    msg <- paste("cfmopwk (open work database) failed with code ",
                 boink$status, ", which supposedly means ",
                 fameStatusMessage(boink$status), sep = "")
    stop(msg)
  }
}

fameModeInt <- function(string){
  modes <- c(read = 1, create = 2, overwrite = 3, update = 4, shared = 5)
  if(is.na(modeNumber <- modes[string]))
    stop("Unknown access.mode")
  else
    return(as.integer(modeNumber))
}

fameDbOpen <- function(dbName, accessMode = "read"){
  boink <- .C("fameOpenDatabase", 
              status = integer(1),
              key = integer(1),
              dbName = as.character(dbName),
              mode = fameModeInt(accessMode),
              PACKAGE = "fame")
  if(boink$status != 0) stop(fameStatusMessage(boink$status))
  return(boink$key)
}

fameDbClose <- function(dbKey){
  status <- .C("fameCloseDatabase", status = integer(1),
               dbKey = as.integer(dbKey), PACKAGE = "fame")$status
  if(status != 0 && status != 2)
    cat(fameStatusMessage(status), "\n")
}

fameDeleteObject <- function(db, fname){
  if(length(db) == 1 && is.numeric(db)){
    ## db is presumably the key to an already-open database
    dbKey <- db
  }
  else {
    dbPath <- getFamePath(db)
    if(is.null(dbPath)) stop(paste("cannot read", db))
    ## make sure a Fame server session is running
    if(!fameRunning()) fameStart()
    ## open database
    dbKey <- as.integer(fameDbOpen(dbPath, accessMode = "shared"))
    on.exit(fameDbClose(dbKey))
  }
  .C("fameDeleteObject",
     status = integer(1),
     dbKey = as.integer(dbKey),
     fname = as.character(fname),
     PACKAGE = "fame")$status
}

getFamePath <- function(dbString){
  ## define fameLocalPath if you have a way to find the path to a database 
  ## return NULL if database corresponding to dbString could not be found
  if(exists("fameLocalPath", mode = "function")){
    path <- fameLocalPath(dbString)
    if(path != dbString) return(path)
  }
  else path <- dbString
  
  if(system(paste("test -r", path), intern = F) == 0)
    path
  else NULL
}

isFameScalar <- function(x){
  !(is.tis(x) || is.ts(x)) && is.atomic(x) && length(x) == 1
}

isScalarOrTis <- function(x){
  is.tis(x) || isFameScalar(x)
}


getfame <- function(sernames, db, save = F, envir = parent.frame(),
                    start = NULL, end = NULL, getDoc = T){
  ## If save = T, the series found are saved in envir using rnames
  ## find path to db
  dbPath <- getFamePath(db)
  if(is.null(dbPath))
    stop(paste("cannot read", db))
  
  ## make sure a Fame server session is running
  if(!fameRunning()) fameStart()

  ## open database
  dbKey <- fameDbOpen(dbPath)
  on.exit(fameDbClose(dbKey))
  
  n <- length(sernames)
  retList <- attList <- vector(n, mode = "list")
  
  if(is.null(rnames <- names(sernames)))
    rnames <- sernames
  names(retList) <- rnames
  
  ## get series attributes
  for(i in 1:n)
    attList[[i]] <- fameWhat(dbKey, sernames[i], getDoc)

  status <- lapply(attList, "[[", "status")
  class  <- lapply(attList, "[[", "class")

  dbName <- basename(gsub(".db", "", dbPath))

  if(any((status == 0) &
         ((class == fameClasses["formula"]) |
          (class == fameClasses["scalar"])))){
    fameCommand(paste('open <access read> "', dbPath, '" as ', dbName, sep = ''),
                silent = T)
    on.exit(fameCommand(paste('close', dbName), silent = T), add = T)
    fameCommand('image date value "<year><mz><dz>:<hhz>:<mmz>:<ssz>"')
    fameCommand('image boolean auto')
    fameCommand('decimal auto')
  }
  
  for(i in 1:n){
    retItem <- list()
    atts <- attList[[i]]
    sername <- sernames[i]

    if(atts$status == 0){
      if(atts$class == fameClasses["scalar"]){
        retItem <- fameCommand(paste("type", sername), capture = T)
        if(attr(retItem, "status") != 0){
          cat("Problem reading", sername, "\n")
          retItem <- list()
          next
        }
        else retItem <- as.vector(retItem)
        isTi <- between(atts$type, 8, 228)
        if(isTi || atts$type == fameTypes["date"]){
          retItem <- strptime(retItem, "%Y%m%d:%H:%M:%S")
          if(isTi)
            retItem <- ti(retItem, tif = fameToTif(atts$type))
        }
        if(atts$type %in% fameTypes[c("boolean", "numeric", "precision")])
          retItem <- eval(parse(text = retItem))

        if(getDoc){
          description(retItem)   <- atts$des
          documentation(retItem) <- atts$doc
        }
      }
      else {
        if(atts$class == fameClasses["formula"]){
          fameCommand(paste("-/", sername, " = ", dbName, "'", sername, sep = ""),
                      silent = T)
          ## dbKey for work database is always 0
          atts <- fameWhat(0, sernames[i], getDoc)
        }

        if(atts$status != 0){
          cat(paste("ERROR retrieving", sername,
                    fameStatusMessage(atts$status)), "\n")
        }
        else {
          if(atts$freq == 232){
            ## a CASE series, we'll read the whole thing
            if(between(atts$type, 8, 228)){ ## a CASE series of FAME dates
              fameFreq <- atts$type
              atts$type <- 6
            }
            range <- atts$range
            obs <- range[3] - range[2] + 1
          }
          else {
            tif <- fameToTif(atts$freq)
            if(tif == 1015){
              ## use reserves frequency rather than bw1wednesday if possible
              if((100*(atts$fyear) + atts$fprd) > 198403)
                tif <- 1011
            }
            ## set date ranges
            dbStart <- ti(c(atts$fyear, atts$fprd), tif)
            dbEnd   <- dbStart + atts$obs - 1
            if(is.null(start)) desiredStart <- dbStart
            else               desiredStart <- ti(start, tif = tif)
            if(is.null(end))   desiredEnd   <- dbEnd
            else               desiredEnd   <- ti(end, tif = tif)
            actualStart <- max(dbStart, desiredStart)
            actualEnd   <- min(dbEnd,   desiredEnd)
            startYear   <- as.integer(year(actualStart))
            startPeriod <- as.integer(cycle(actualStart))
            obs         <- as.integer(actualEnd - actualStart + 1)
            if(obs < 1) next
            range <- fameRange(freq = atts$freq,
                               startYear = startYear, startPeriod = startPeriod,
                               obs = obs)$range
          }
          
          z <- switch(as.character(atts$type),
                      "1" = {
                        .C("fameReadNumericSeries",
                           status      = integer(1),
                           dbKey       = atts$dbKey,
                           name        = atts$name,
                           range       = as.integer(range),
                           data        = double(obs),
                           PACKAGE = "fame")
                      },
                      "5" = {
                        .C("fameReadPrecisionSeries",
                           status      = integer(1),
                           dbKey       = atts$dbKey,
                           name        = atts$name,
                           range       = as.integer(range),
                           data        = double(obs),
                           PACKAGE = "fame")
                      },
                      "3" = {
                        .C("fameReadIntegerSeries",
                           status      = integer(1),
                           dbKey       = atts$dbKey,
                           name        = atts$name,
                           range       = as.integer(range),
                           data        = logical(obs),
                           PACKAGE = "fame")
                      },
                      "6" = {  ## fame dates
                        .C("fameReadIntegerSeries",
                           status      = integer(1),
                           dbKey       = atts$dbKey,
                           name        = atts$name,
                           range       = as.integer(range),
                           data        = integer(obs),
                           PACKAGE = "fame")
                      },
                      "4" = { ## strings
                        zz <- .C("fameGetStringLengths",
                                 status      = integer(1),
                                 dbKey       = atts$dbKey,
                                 name        = atts$name,
                                 range       = as.integer(range),
                                 lengths     = integer(obs),
                                 PACKAGE = "fame")
                        if(zz$status != 0){
                          cat(fameStatusMessage(z$status))
                          break
                        }
                        maxlen <- max(3, max(zz$lengths) + 1)
                        zzz <- .C("fameReadStringSeries",
                                  status      = integer(1),
                                  dbKey       = atts$dbKey,
                                  name        = atts$name,
                                  range       = as.integer(range),
                                  data        = rep(blanks(maxlen), obs),
                                  strlength   = as.integer(maxlen),
                                  PACKAGE = "fame")
                        zzz$data <- stripBlanks(zzz$data)
                        zzz
                      },
                      { ## default -- crap out
                        list(status = 16)
                      })
          if(z$status == 0){
            if(atts$freq == 232){
              if(atts$type == 6)
                retItem <- fameDateToTi(z$data, fameFreq)
              else
                retItem <- z$data
            }
            else {
              retItem <- tis(z$data, start = actualStart)
              if(atts$basis > 0) 
                attr(retItem, "basis") <- c("daily", "business")[atts$basis]
              if(atts$observ > 0) 
                attr(retItem, "observed") <-
                  c("beginnin", "ending", "averaged", "summed", "annualized",
                    "formula", "high", "low")[atts$observ]
            }
            if(getDoc){
              description(retItem)    <- atts$des
              documentation(retItem) <- atts$doc
            }
          }
          else
            retItem <- list()
        }
      }
    }
    retList[[i]] <- retItem
  }
  retLengths <- lapply(retList, length)
  zz <- retList[retLengths > 0]
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
  dbPath <- getFamePath(db)
  if(is.null(dbPath)) dbPath <- db
  
  if(access == "append"){
    ## for compatibility with old ffi version of putfame
    access <- "shared"
  }
  if(missing(access)){
    if(file.exists(dbPath))
      access <- "shared"
    else
      access <- "create"
  }
  ## make sure a Fame server session is running
  if(!fameRunning()) fameStart()

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
      if(!isScalarOrTis(obj))
        stop(paste(rname, "is not a fame scalar or a tis series"))
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
  if(!all(sapply(zz, isScalarOrTis)))
    stop("non-scalar, non-tis argument")

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

      for(i in 1:sercols)
		z[[i.z[i]]] <- ser[,i]
    }
    else {
      znames[i.z] <- fameNames[i.sl]
      z[[i.z]] <- ser
    }
    if(any(nchar(znames[i.z]) == 0))
      stop("unnamed scalar or series")
  }

  ## find scalar and series elements in z 
  scalar <- sapply(z, isFameScalar)
  scalarIndex <- (1:length(z))[scalar]
  seriesIndex <- (1:length(z))[!scalar]
  
  dbKey <- fameDbOpen(dbPath, accessMode = access)
  ## We may have to open and close the database multiple times (don't ask why,
  ## just know that FAME sucks).  To prevent reopening from doing something
  ## horrible, like wiping out the database (which reopening in "overwrite"
  ## would do) or throwing an error (which reopening in "create" would do),
  ## we open the database once in the desired access mode, then immediately
  ## close it and change the access mode to "shared" for future openings.
  if(access %in% c("create", "overwrite")) access <- "shared"
  fameDbClose(dbKey)
  
  if(any(scalar)){ ## write the scalars out first
    fameCommand(paste('open <access ', access, '> "', dbPath,
                      '" as targetdb', sep = ''), silent = T)
    for(i in scalarIndex)
      fameWriteScalar(dbPath, znames[i], z[[i]], update = update)
    fameCommand('close targetdb')
    
  }
  if(any(!scalar)){ ## now write out the tis series
    dbKey <- fameDbOpen(dbPath, accessMode = access)
    on.exit(fameDbClose(dbKey))
    for(i in seriesIndex){
      fameWriteSeries(dbKey, znames[i], z[[i]], update = update,
                      checkBasisAndObserved = checkBasisAndObserved)
    }
  }
}

fameWriteScalar <- function(dbPath, fname, scalar, update = T){
  ## if update is FALSE or there is no existing object named 'fname', put
  ## overwrite on to force creation of a new object along with any documentation
  ## and description attributes.
  ## Note that the database being written to is 'targetdb'.  This should be
  ## the same database as specified by 'dbPath', but 'dbPath' is used only
  ## when searching, as the CHLI only allows searches on databases via the
  ## dbkey, and it provides no way of getting the dbkey for a database opened
  ## via the cfmfame() function employed by fameCommand()
  if(is.ti(scalar))              type <- "date"
  else if(is.numeric(scalar))    type <- "precision"
  else if(is.logical(scalar))    type <- "boolean"
  else if(is.character(scalar))  type <- "string"
  else stop("scalar must be ti, numeric, logical or character type")
  
  wl <- fameWildlist(db = dbPath, wildString = fname, nMax = 2)
  nFound <- length(wl[[1]])
  if(nFound > 1) stop("found multiple objects with same name")
  
  if(update && nFound > 0){
    dbType <- tolower(wl$type[1])
    if(dbType != type) stop("dbType must match scalar type when update = T")
    cmd <- paste("update !targetdb'", fname, " =", sep = "")
  }
  else {
    overwriteState <- fameCommand("type @overwrite", capture = T)
    fameCommand("overwrite TRUE")
    on.exit(fameCommand(paste("overwrite", overwriteState)), add = T)
    cmd <- paste("scalar !targetdb'", fname, ":", type, " =", sep = "") 
  }

  switch(type,
         date = {
           fameCommand(paste("frequency", tifToFameName(scalar)))
           fameCommand(paste(cmd, fameDateString(scalar)))
         },
         precision = {
           fameCommand(paste(cmd, format(scalar, digits =14)))
         },
         boolean = {
           fameCommand(paste(cmd, scalar))
         },
         string = {
           fameCommand(paste(cmd, " \"", scalar, "\"", sep = ""))
         })
  if(nFound == 0 || update == F){
    if(!is.null(desc <- description(scalar)))
      fameCommand(paste("description(", fname, ") = \"", desc, "\"", sep = ""))
    if(!is.null(doc <- documentation(scalar)))
      fameCommand(paste("documentation(", fname, ") = \"", doc, "\"", sep = ""))
  }
}

fameWriteSeries <- function(dbKey, fname, ser, update = F,
                              checkBasisAndObserved = F){
  ## Write the tis (TimeIndexedSeries) ser as fname in the database given by dbKey.
  ## If an object named fname already exists in the database and update == T,
  ## the frequency, observed, and basis attributes of ser are checked for
  ## consistency with the existing object, then the range covered by ser is
  ## written to the database.  If update == F, any existing series called fname
  ## will be deleted before writing ser to the database.
  if(!inherits(ser, "tis")) stop("not a time indexed series")
  if(!is.null(nc <- ncol(ser)) && !is.na(nc) && nc != 1)
    stop("not a univariate series")
  
  ## see if ser is already in the database and get info about it
  wl <- fameWildlist(dbKey, wildString = fname, nMax = 2, charMode = F)
  nFound <- length(wl[[1]])
  if(nFound > 1) stop("found multiple objects with same name")

  if(update && nFound > 0) type <- wl$type[1]
  else {
    if(mode(ser) == "logical") type <- fameTypes["boolean"]
    else                       type <- fameTypes["precision"]
  }
  storage.mode(ser) <- "double"
  
  ## basis
  if(is.null(basis <- basis(ser)))
    basis <- 0     ## let the C code assign a basis attribute
  else 
    basis <- c(day=1, daily=1, business=2)[basis]
  
  ## observ
  if(is.null(observ <- observed(ser)))
    observ <- 0    ## let the C code assign an observed attribute
  else
    observ <- fameObserveds[observ]

  ## desc and doc 
  ## Note that doc and desc attributes are ignored when updating existing series
  if(is.null(desc <- description(ser)  )) desc <- ""
  if(is.null(doc  <- documentation(ser))) doc  <- ""
  z <- .C("fameWriteRange",
          status      = integer(1),
          dbKey       = as.integer(dbKey),
          fname       = as.character(fname),
          freq        = as.integer(tifToFame(tif(ser))),
          type        = as.integer(type),
          basis       = as.integer(basis),
          observ      = as.integer(observ),
          startYear   = as.integer(year(start(ser))),
          startPeriod = as.integer(cycle(start(ser))),
          len         = as.integer(length(ser)),
          desc        = as.character(desc),
          doc         = as.character(doc),
          data        = as.numeric(ser),
          update      = as.integer(update),
          checkBasisAndObserved = as.integer(checkBasisAndObserved),  
          NAOK = T,
          PACKAGE = "fame")
  status <- z$status
  if(status != 0) cat(fameStatusMessage(status))
  if(status != 0) stop()
  invisible(status)
}

fameDateToTi <- function(fameDates, freq = tifToFame("daily")){
  if(!(is.numeric(freq) && freq < 999))
    freq <- tifToFame(freq)
  firstYmd <- fameYmd(fameDates[1], freq)
  firstTi <- ti(firstYmd, tif = fameToTif(freq))
  firstTi + (fameDates - fameDates[1])
}

fameYmd <- function(fameDate, fameFreq = tifToFame("daily")){
  ## make sure a Fame server session is running
  if(!fameRunning()) fameStart()
  z <- .C("yearMonthDayFromFameDate",
          status   = integer(1),
          freq     = as.integer(fameFreq),
          fameDate = as.integer(fameDate),
          year     = integer(1),
          month    = integer(1),
          day      = integer(1),
          PACKAGE  = "fame")
  if(z$status != 0){
    cat(fameStatusMessage(z$status))
    stop()
  }
  10000*z$year + 100*z$month + z$day
}

fameDate <- function(inDate = today(), tif = "daily"){
  if(missing(tif) && is.ti(inDate)) tif <- tif(inDate)
  tiDate <- ti(inDate, tif = tif)
  ## make sure a Fame server session is running
  if(!fameRunning()) fameStart()
  z <- .C("fameDateFromYearMonthDay",
          status   = integer(1),
          freq     = as.integer(tifToFame(tif)),
          fameDate = integer(1),
          year     = as.integer(year(tiDate)),
          month    = as.integer(month(tiDate)),
          day      = as.integer(day(tiDate)),
          PACKAGE  = "fame")
  if(z$status != 0){
    cat(fameStatusMessage(z$status))
    stop()
  }
  z$fameDate
}

fameRange <- function(freq, startYear = -1, startPeriod = -1,
                      endYear = -1, endPeriod = -1,
                      obs = -1){
  ## make sure a Fame server session is running
  if(!fameRunning()) fameStart()
  z <- .C("fameSetRange",
          status = integer(1),
          freq  = as.integer(freq),
          fyear = as.integer(startYear),
          fprd  = as.integer(startPeriod),
          lyear = as.integer(endYear),
          lprd  = as.integer(endPeriod),
          obs   = as.integer(obs),
          range = integer(3),
          PACKAGE = "fame")
  if(z$status != 0){
    cat(fameStatusMessage(z$status))
    stop()
  }
  z
}


fameWhat <- function(dbKey, fname, getDoc = F){
  getDoc <- as.integer(as.logical(getDoc))
  ## read low-level information about an object in a Fame database
  z <- .C("fameWhat",
          status = integer(1),
          dbKey  = as.integer(dbKey),
          name   = as.character(fname),
          class  = integer(1),
          type   = integer(1),
          freq   = integer(1),
          basis  = integer(1),
          observ = integer(1),
          fyear  = integer(1),
          fprd   = integer(1),
          lyear  = integer(1),
          lprd   = integer(1),
          obs    = as.integer(-1),
          range  = integer(3),
          getDoc = getDoc,
          des    = blanks(256*getDoc),
          doc    = blanks(256*getDoc),
          PACKAGE = "fame")
  if(getDoc){
    z$des <- stripBlanks(z$des)
    z$doc <- stripBlanks(z$doc)
  }
  else z$des <- z$doc <- character(0)
  z
}

fameWhats <- function(db, fname, getDoc = T){
  if(length(db) == 1 && is.numeric(db)){
    ## db is presumably the key to an already-open database
    dbKey <- db
  }
  else {
    dbPath <- getFamePath(db)
    if(is.null(dbPath)) stop(paste("cannot read", db))
    ## make sure a Fame server session is running
    if(!fameRunning()) fameStart()
    ## open database
    dbKey <- as.integer(fameDbOpen(dbPath))
    on.exit(fameDbClose(dbKey))
  }
  ## higher level (and slower) version of fameWhat()
  z <- fameWhat(dbKey, fname, getDoc)
  if(z$status != 0){
    warning(fameStatusMessage(z$status))
    return(NULL)
  }

  zz <- list(name     = tolower(z$name),
             class    = names(fameClasses[  match(z$class,  fameClasses)]),
             type     = names(fameTypes[match(z$type, fameTypes)]))
  if(!is.na(cTif <- tifName(fameToTif(z$type)))){
    zz$tif <- cTif
  }
  
  if(zz$class == "series"){
    zz <- c(zz, 
            basis    = names(fameBasiss[   match(z$basis,  fameBasiss)]),
            observed = names(fameObserveds[match(z$observ, fameObserveds)]),
            length   = z$obs)
    zz$start <- ti(c(z$fyear, z$fprd), tif = fameToTif(z$freq))
  }
  if(getDoc){
    zz$des <- z$des
    zz$doc <- z$doc
  }
  zz
}

fameWildlist <- function(db, wildString = "?", nMax = 1000, charMode = T){
  ## returns a list giving the name, class, type, and frequency of the objects
  ## in db with names that match wildString
  if(length(db) == 1 && is.numeric(db)){
    ## db is presumably the key to an already-open database
    dbKey <- db
  }
  else {
    dbPath <- getFamePath(db)
    if(is.null(dbPath)) stop(paste("cannot read", db))
    ## make sure a Fame server session is running
    if(!fameRunning()) fameStart()
    ## open database
    dbKey <- as.integer(fameDbOpen(dbPath))
    on.exit(fameDbClose(dbKey))
  }

  ## internal functions
  initWildcard <- function(dbKey, wildString = "?"){
    status <- .C("fameInitializeWildcard",
                 status = integer(1),
                 dbKey  = as.integer(dbKey),
                 wilnam = as.character(wildString),
                 PACKAGE = "fame")$status
    if(status != 0) cat(fameStatusMessage(status), "\n")
    return(status)
  }
  blankString <- blanks(255) ## create this only once
  getNextMatch <- function(dbKey){
    .C("fameGetNextMatch",
       status = integer(1),
       dbKey  = as.integer(dbKey),
       name   = blankString,
       class  = integer(1),
       type   = integer(1),
       freq   = integer(1),
       PACKAGE = "fame")
  }
  ## end of internal functions
  
  status <- initWildcard(dbKey, wildString)
  if(status != 0) stop(fameStatusMessage(status))
  
  zName <- character(nMax)
  zClass <- zType <- zFreq  <- numeric(nMax)
  nFound <- 0
  while(nFound < nMax){
    nextMatch <- getNextMatch(dbKey)
    status <- nextMatch$status
    if(nextMatch$status == 0){
      nFound <- nFound + 1
      zName[nFound]  <- nextMatch$name
      zClass[nFound] <- nextMatch$class
      zType[nFound]  <- nextMatch$type
      zFreq[nFound]  <- nextMatch$freq
    }
    else break
  }
  if(status == 0) cat("Number of matches exceeded nMax =", nMax, "\n")
  else if(status != 13) stop(fameStatusMessage(status))

  if(nFound == 0)
    return(list(name = character(0),
                class = numeric(0),
                type = numeric(0),
                freq  = numeric(0)))
  
  z <- list(name = tolower(zName[1:nFound]),
            class = zClass[1:nFound],
            type = zType[1:nFound],
            freq  = zFreq[1:nFound])
  
  if(nFound > 0 && charMode){
    z$class <- names(fameClasses)[z$class]
    types <- z$type
    z$type <- names(fameTypes)[types + 1]
    isDateType <- between(types, 8, 228)
    z$type[isDateType] <- "date"
    z$freq[isDateType] <- types[isDateType]
    isCaseSeries <- z$freq == 232
    z$freq  <- tifName(fameToTif(z$freq))
    z$freq[isCaseSeries] <- "case"
  }
  z
}

fameStatusMessage <- function(code){
  switch(as.character(code),
	 "0"   = "Success.",
	 "1"   = "HLI has already been initialized.",
	 "2"   = "HLI has not been initialized.",
	 "3"   = paste("HLI has already been finished and cannot be", 
	     "reinitialized in the same session."),
	 "4"   = "A bad file name was given.",
	 "5"   = paste("A bad or unauthorized file access mode was given", 
	     "or the given data base is not open for the requested access."),
	 "6"   = "A bad data base key was given.",
	 "8"   = "A bad starting year or period was given for a range.",
	 "9"   = "A bad ending year or period was given for a range.",
	 "10"  = "A bad number of observations was given for a range.",
	 "13"  = "The given object does not exist.",
	 "14"  = "A bad range was given.",
	 "15"  = "The target object already exists.",
	 "16"  = paste("A bad object type was given or the given object", 
	     "has the wrong type."),
	 "17"  = paste("A bad frequency was given or the given object has", 
	     "the wrong frequency."),
	 "18"  = "The oldest data has been truncated.",
	 "20"  = "The data base has not been posted or closed.",
	 "21"  = "The file is already in use.",
	 "22"  = "The file is not a FAME data base.",
	 "23"  = "Trying to read or update a file that does not exist.",
	 "24"  = "Trying to create a file that already exists.",
	 "25"  = paste("The name given is not a legal FAME name or is", 
	     "a FAME reserved word."),
	 "26"  = paste("A bad object class was given or the given object", 
	     "has the wrong class."),
	 "27"  = "A bad OBSERVED attribute was given.",
	 "28"  = "A bad BASIS attribute was given.",
	 "29"  = "The data object already exists.",
	 "30"  = "A bad month was given.",
	 "31"  = "A bad fiscal year label was given.",
	 "32"  = "A bad missing value type was given.",
	 "33"  = "A bad value index was given.",
	 "34"  = paste("Wildcarding has not been initialized for the data", 
	     "base or has since been invalidated."),
	 "35"  = "A bad number of characters was given.",
	 "36"  = "A bad growth factor was given.",
	 "37"  = "Maximum num of files already open or no disk space is available.",
	 "38"  = "Can't update or share an old data base.",
	 "39"  = "The data base must be posted.",
	 "40"  = "Can't write to a special data base.",
	 "41"  = "A bad flag was given.",
	 "42"  = "Can't perform operation on packed data base",
	 "43"  = "The data base is not empty",
	 "44"  = "A bad attribute name was given.",
	 "45"  = "A duplicate was ignored.",
	 "46"  = "A bad year was given.",
	 "47"  = "A bad period was given.",
	 "48"  = "A bad day was given.",
	 "49"  = "A bad date was given.",
	 "50"  = "A bad date selector was given.",
	 "51"  = "A bad date relation value was given.",
	 "52"  = "A bad hour, minute or second was given.",
	 "53"  = "Unauthorized CPU ID or hardware type",
	 "54"  = "Expired dead date.",
	 "55"  = "Unauthorized product.",
	 "56"  = "A bad number of units was given",
	 "57"  = "This operation not allowed in current context",
	 "58"  = "This object is locked by the FAME session",
	 "59"  = paste("Could not connect to specified host to open the", 
	     "data base.  Possible reasons include: Bad host name, user or", 
	     "password.  Server not running on specified host.", 
	     " Also used for lost connection."),
	 "60"  = "FAME process has terminated",
	 "61"  = paste("Data base server process on other machine terminated", 
	     "unexpectedly."),
	 "62"  = paste("Access to a remote data base has been temporarily", 
	     "suspended. Try again later."),
	 "63"  = paste("Remote host does not support current intermachine", 
	     "protocol version"),
	 "64"  = "Remote host client limit exceeded.",
	 "65"  = paste("Bad user name or password in file spec for remote", 
	     "host, or client not authorized to use remote host"),
	 "66"  = "Could not start server process on remote host",
	 "67"  = "Bad option",
	 "68"  = "Bad value for this option",
	 "69"  = "Operation not supported on this data base",
	 "70"  = "A bad length was given.",
	 "71"  = "A NULL ptr was given.", 
	 "72"  = "Invalid for read only hli",
	 "73"  = paste("Data base contains new features unknown to this", 
	     "older HLI release.  Link with a newer version of the HLI."),
	 "74"  = paste("An invalid name was specified for a GLNAME or", 
	     "GLFORMULA.  Check the %k prefix of the name and the", 
	     "dimension of the data base.  For %1 names, the rest of the", 
	     "name must also be a valid name.  Also used for invalid", 
	     "aliase of a GLNAME or GLFORMULA."),
	 "75"  = paste("A fatal I/O error or termination of server has", 
	     "caused the channel to be closed."),
	 "76"  = paste("Call to cfmopre by a client that already has a", 
	     "dbKey for a KIND REMOTE channel to the mcadbs server"),
	 "77"  = "cfmopwk called when a work data base is already open",
	 "78"  = "FRDB user license cannot be accquired.",
	 "511" = "HLI internal failure.",
	 "513" = paste("Error from a FAME-like server. Call cfmferr for the", 
	     "text of the message HFAMER is > 512 for compatiblity with",
	     "cfmfame in earlier releases."),
	 "Unknown status code")
}

fameClasses   <- c(series = 1, scalar = 2, formula = 3)
fameTypes     <- c(undefined = 0, numeric = 1, namelist = 2,
                   boolean = 3, string = 4, precision = 5, date = 6)
fameBasiss    <- c(undefined = 0, daily = 1, business = 2)
fameObserveds <- c(undefined = 0, beginning = 1, ending = 2, averaged = 3,
                   summed = 4, annualized = 5, formula = 6, high = 7, low = 8)
