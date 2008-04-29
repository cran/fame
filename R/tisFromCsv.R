tisFromCsv <- function(csvFile,
                       dateCol = "date8",
                       dateFormat = "%Y%m%d",
                       tif = NULL,
                       defaultTif = "business",
                       save = F,
                       envir = parent.frame(),
                       naNumber = NULL,
                       tolerance = sqrt(.Machine$double.eps),
                       ...){
  ## csvFile is the path to a csv file with column names across the top.
  ## Everything but the first row (the column names) should be numeric, and there
  ## must as many column names (enclosed in quotes) as there are columns.
  ## The column named by dateCol should contain dates in the dateFormat format.
  ##
  ## The function reads in all the data and returns a list of named tis
  ## (Time Indexed Series) objects.  The names of the list are the column
  ## names from the csvFile.
  ## The naWindow() function is applied to each series to chop off leading and
  ## trailing NA observations, so the series in the list may not all start and
  ## end on the same dates.
  retList <- list()
  zdf <- read.csv(csvFile, as.is = T, ...)
  zNames <- names(zdf)
  dateColIndex <- match(tolower(dateCol), tolower(zNames), nomatch = NA)
  if(is.na(dateColIndex))
    stop(paste(csvFile, "does not have a column named", dateCol, "\n"))
  if(all(toupper(zNames) == zNames)) names(zdf) <- tolower(zNames)
  
  zDateStrings <- as.character(zdf[[dateColIndex]])
  z <- as.matrix(zdf[,unlist(lapply(zdf, is.numeric)), drop = F])
  if(!is.null(naNumber)){
    naSpots <- (1:length(z))[abs(z - naNumber) <= tolerance]
    naSpots <- naSpots[!is.na(naSpots)]
    z[naSpots] <- NA
  }
  cn <- colnames(z) 
  if(NCOL(z) == 0) stop("No non-NA values in file")

  if(tolower(dateFormat) == "excel")
    dateTimes <- POSIXct(jul(as.ssDate(zdf[[dateColIndex]])))
  else {
    if(length(grep("%d", dateFormat)) == 0){
      dateFormat <- paste(dateFormat, "%d")
      zDateStrings <- paste(zDateStrings, "1")
    }
    dateTimes <- as.POSIXct(strptime(zDateStrings, format = dateFormat))
  }
  if(!is.null(tif))
    dtTi <- ti(dateTimes, tif = tif)
  else {
    diffSeconds <- median(diff(unclass(dateTimes)))
    freq <- round((365.25 * 60*60*24)/diffSeconds)
    if(is.na(freq)) freq <- tif2freq(defaultTif)

    if(freq > 365){  ## maybe intraday
      if((diffSeconds %% 3600) == 0){
        tif <- hourly(diffSeconds / 3600)
      } else if((diffSeconds %% 60) == 0){
        tif <- minutely(diffSeconds / 60)
      } else tif <- secondly(diffSeconds)
    } else {
      if(freq == 365){
        if(all(between(dayOfWeek(dtJul), 2, 6)))
          tif <- "business"
        else tif <- "daily"
      } else tif <- freq2tif(freq)
    }
    dtTi <- ti(dateTimes, tif = tif)
    dtJul <- floor(jul(dateTimes))
    
    if(median(abs(jul(dtTi) - dtJul)) > 0.5){ ## could be wrong ti
      if(freq == 52){
        newTif <- tif("wsunday") + dayOfWeek(max(dtJul)) - 1
        dtTi <- ti(dtJul, tif = newTif)
      }
      if(freq == 26){
        newTif <- tif("bw1sunday") + dayOfPeriod(max(dtJul), "bw1sunday") - 1
        if(tifName(newTif) == "bw1wednesday" && ("mra" %in% groups()))
          newTif <- "reserves"
        dtTi <- ti(dtJul, tif = newTif)
      }
      if(freq == 6){
        newTif <- tif("bmdecember") - (month(max(dtJul)) %% 2)
        dtTi <- ti(dtJul, tif = newTif)
      }
      if(freq == 4){
        newTif <- tif("qoctober") + ((2 + month(max(dtJul))) %% 4)
        dtTi <- ti(dtJul, tif = newTif)
      }
      if(freq == 2){
        newTif <- tif("sannjuly") + ((5 + month(max(dtJul))) %% 6)
        dtTi <- ti(dtJul, tif = newTif)
      }
      if(freq == 1){
        newTif <- tif("annjanuary") - 1 + month(max(dtJul))
        dtTi <- ti(dtJul, tif = newTif)
      }
    }
  }
  zStart <- dtTi[1]
  zEnd   <- tail(dtTi, 1)
  zSeries <- tis(matrix(NA, zEnd - zStart + 1, ncol(z)), start = zStart)
  zSeries[dtTi,] <- z
  class(zSeries) <- "tis"
  colnames(zSeries) <- colnames(z)
  
  retList <- lapply(columns(zSeries), naWindow)
  if(save) assignList(retList, env = envir)
  gc()
  
  if(save)
    invisible(retList)
  else
    retList
}
