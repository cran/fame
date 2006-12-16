tisFromCsv <- function(csvFile,
                       dateCol = "date8",
                       dateFormat = "%Y%m%d",
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
  if(all(toupper(zNames) == zNames)) zNames <- tolower(zNames)
  names(zdf) <- zNames

  zDateStrings <- as.character(zdf[[dateColIndex]])
  zdf <- zdf[, -dateColIndex, drop = F]
  z <- as.matrix(zdf[,unlist(lapply(zdf, is.numeric)), drop = F])
  if(!is.null(naNumber)){
    naSpots <- (1:length(z))[abs(z - naNumber) <= tolerance]
    naSpots <- naSpots[!is.na(naSpots)]
    z[naSpots] <- NA
  }
  cn <- colnames(z) 

  dtJul <- jul(zDateStrings, format = dateFormat)
  if(NCOL(z) == 0) stop("No non-NA values in file")
  freq <- round(365.25/median(diff(dtJul)))
  if(is.na(freq)) freq <- tif2freq(defaultTif)
  ## get nearest ti for each dt
  dtTime <- time(dtJul) - 1/(2*freq)
  if(freq == 365 && all(between(dayOfWeek(dtJul), 2, 6)))
    dtTi <- ti(dtTime, tif = "business")
  else 
    dtTi <- ti(dtTime, freq = freq)
  
  if(median(abs(jul(dtTi) - dtJul)) > 0.5){ ## could be wrong ti
    if(freq == 52){
      newTif <- tif("wsunday") + dayOfWeek(max(dtJul)) - 1
      dtTi <- ti(dtTime, tif = newTif)
    }
    if(freq == 26){
      newTif <- tif("bw1sunday") + dayOfPeriod(max(dtJul), "bw1sunday") - 1
      if(tifName(newTif) == "bw1wednesday" && ("mra" %in% groups()))
        newTif <- "reserves"
      dtTi <- ti(dtTime, tif = newTif)
    }
    if(freq == 6){
      newTif <- tif("bmdecember") - (month(max(dtJul)) %% 2)
      dtTi <- ti(dtTime, tif = newTif)
    }
    if(freq == 4){
      newTif <- tif("qoctober") + ((2 + month(max(dtJul))) %% 4)
      dtTi <- ti(dtTime, tif = newTif)
    }
    if(freq == 2){
      newTif <- tif("sannjuly") + ((5 + month(max(dtJul))) %% 6)
      dtTi <- ti(dtTime, tif = newTif)
    }
    if(freq == 1){
      newTif <- tif("annjanuary") - 1 + month(max(dtJul))
      dtTi <- ti(dtTime, tif = newTif)
    }
  }
  zSeries <- tis(NA, start = dtTi[1], end = dtTi[length(dtTi)])
  zSeries[dtTi,] <- z
  retList <- lapply(columns(zSeries), naWindow)
  if(save){
    for(rln in names(retList))
      assign(rln, retList[[rln]], env = envir)
  }
  gc()

  if(save)
    invisible(retList)
  else
    retList
}
