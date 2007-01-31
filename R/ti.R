## Date representations:
## For yp, c(year, period) is encoded as year*1e8 + period
## For ymd, c(year, month, day, hour, minute, second) is encoded as
##    year*10000 + month *100 + day + (3600*hour + 60*minute + second)/86400
## For jul, c(julianDay, hour, minute, second) is encoded as
##    day + (3600*hour + 60*minute + second)/86400
## For ti (Time Index), c(tif, period) is encoded as tif*1e10 + period
##
## TIFs (Time Index Frequency)
## A tif can be a number between 1001 and 4900, or a string giving the
## frequency name.  tifList() returns a list of the non-intraday frequencies.
## Non-intraday frequency numbers are in the 1001 - 1050 range.
## Intraday frequencies like hourly(3) (every third hour) or secondly(15)
## (every 15'th second) can also be used.  They are encoded as follows:
##      hourly(n)   -->  2000 + n
##      minutely(n) -->  3000 + n
##      secondly(n) -->  4000 + n


as.ti  <- function(x) structure(x, class = "ti")
as.jul <- function(x) structure(x, class = "jul")

year   <- function(x, ...) ymd(x, ...) %/% 10000
month  <- function(x, ...) (ymd(x, ...) %/% 100) %% 100
day    <- function(x, ...) ymd(x, ...) %% 100
quarter <- function(x, ...) (month(x, ...) + 2) %/% 3

##From julian date to others
jul2time <- function(jul){
  y <- jul2ymd(jul) %/% 10000
  day <- unclass(1 + jul - ymd2jul(10000*y + 101))
  daysInYear <- 365 + isLeapYear(y)
  return(y + day/daysInYear)
}

jul2ti <- function(jul, tif){
  tifLen <- length(tif)
  if(tifLen > 1 && length(uniq <- unique(tif)) > 1){
    n <- max(length(jul), tifLen)
    juln <- rep(jul, length = n)
    tifn <- rep(tif, length = n)
    ans <- as.ti(unclass(juln))
    for(u in uniq){
      index <- tifn == u
      ans[index] <- julToTi(juln[index], u)
    }
    return(ans)
  }
  else return(julToTi(jul, tif[1]))
}

jul2ymd <- function(jul){
  jul <- unclass(jul)
  seconds <- round((jul*86400) %% 86400)
  rjul <- jul %/% 1
  j <- as.vector(rjul) - 1721119
  y <- (4*j - 1) %/% 146097
  j <- 4*j - 1 - 146097*y
  d <- j %/% 4
  j <- (4*d + 3) %/% 1461
  d <- 4*d + 3 - 1461*j
  d <- (d + 4) %/% 4
  m <- (5*d - 3) %/% 153
  d <- 5*d - 3 - 153*m
  d <- (d + 5) %/% 5
  y <- 100*y + j
  y <- y + as.numeric(m > 9)
  m <- m + ifelse(m < 10, 3, -9)
  ans <- 10000*y + 100*m + d + seconds/86400
  attributes(ans) <- attributes(jul)
  return(ans)
}

jul2yp <- function(jul, tif) ti2yp(jul2ti(jul, tif))

## From ti to others
ti2jul <- function(ti){
  tif <- tif(ti)
  tifLen <- length(tif)
  if(tifLen > 1 && length(uniq <- unique(tif)) > 1){
    ans <- as.jul(unclass(ti))
    for(u in uniq){
      index <- tif == u
      ans[index] <- tiToJul(ti[index])
    }
    return(ans)
  }
  else return(tiToJul(ti))
}

ti2time <- function(x) jul2time(ti2jul(x))

ti2ymd <- function(ti){
  tif <- tif(ti)
  if(length(uniq <- unique(tif)) > 1){
    ans <- unclass(ti)
    for(u in uniq){
      index <- tif == u
      ans[index] <- tiToYmd(ti[index])
    }
    return(ans)
  }
  else return(tiToYmd(ti))
}

ti2yp <- function(x){
  y <- ti2ymd(x) %/% 10000
  p <- x + 1 - yp2ti(1e8*y + 1, tif(x))
  return(1e8*y + unclass(p))
}

## From time [decimal time as returned by time()] to others
time2jul <- function(time){
  rawTime <- unclass(time)
  zeros <- numeric(length(rawTime))
  y <- floor(rawTime + 1e-8)
  secondsInYear <- 86400*(zeros + 365 + isLeapYear(y))
  d <- ceiling(floor(secondsInYear*(rawTime - y) + 0.5)/86400)
  return(ymd2jul(10000*(y-1) + 1231) + d)
}

time2ti  <- function(time, tif) jul2ti(time2jul(time), tif)

time2ymd <- function(time)      jul2ymd(time2jul(time))

time2yp  <- function(time, tif) jul2yp(time2jul(time), tif)

##From ymd to others
ymd2jul <- function(ymd){
  ucymd <- unclass(ymd)
  seconds <- round(86400 * (ucymd %% 1))
  rYmd <- floor(ucymd + 0.5/86400)
  y <- rYmd %/% 10000
  m <- (rYmd - y*10000) %/% 100
  d <- rYmd %% 100
  y  <- y + (y < 0)
  jy <- y - (m <= 2)
  jm <- m + 1 + 12*(m <= 2)
  jul <- floor(365.25*jy) + floor(30.6001*jm) + d + 1720995;
  addfac <- (2 - floor(0.01*jy) + floor(0.0025*jy))
  i <- ( + 31*(m + 12*y)) >= 588829
  jul[i] <- jul[i] + addfac[i]
  return(jul + seconds/86400)
}

ymd2ti <- function(ymd, tif){
  tifLen <- length(tif)
  if(length(uniq <- unique(tif)) > 1){
    n <- max(length(ymd), tifLen)
    ymdn <- rep(ymd, length = n)
    tifn <- rep(tif, length = n)
    ans <- as.ti(unclass(ymdn))
    for(u in uniq){
      index <- tifn == u
      ans[index] <- ymdToTi(ymdn[index], u)
    }
    return(ans)
  }
  else return(ymdToTi(ymd, tif[1]))
}

ymd2time <- function(ymd) jul2time(ymd2jul(ymd))

ymd2yp <- function(ymd, tif) ti2yp(ymd2ti(ymd, tif))

## From yp (year*1e8 + periodOfYear) conversions
yp2time <- function(yp, tif, offset = 1)
  time(yp2ti(yp, tif), offset)

yp2jul <- function(yp, tif, offset = 1)
  jul(yp2ti(yp, tif), offset)

yp2ti <- function(yp, tif){
  if(!is.time(yp/1e8)) stop("Bad yp arg")
  y <- yp %/% 1e8
  p <- yp %%  1e8
  ymd2ti(10000*y + 101, tif) + p - 1
}

yp2ymd <- function(yp, tif, offset = 1)
  ymd(yp2ti(yp, tif), offset)


## Object oriented wrappers
## constructors for class 'jul'
is.jul <- function(x) inherits(x, "jul")

jul <- function(x, ...) UseMethod("jul")

jul.jul <- function(x, ...) x

jul.ssDate <- function(x, ...) jul(18991230) + unclass(x)

jul.ti <- function(x, offset = 1, ...){
  z <- stripClass(x, "ti")
  rx <- x
  rx[] <- floor(unclass(x) + .Machine$double.eps)
  j1 <- unclass(ti2jul(rx))
  if(missing(offset)){
    j2 <- unclass(ti2jul(rx + 1))
    secondsPerPeriod <- 86400*(j2 - j1)
    fracPart <- round((z %% 1) * secondsPerPeriod)/secondsPerPeriod
    z[] <- fracPart*j2 + (1 - fracPart)*j1
  }
  else {
    if(!between(offset, 0, 1)) stop("offset must be in [0,1]")
    if(offset == 1)
      z[] <- j1
    else {
      j0 <- unclass(ti2jul(rx - 1))
      oneSecond <- 1/86400
      if(offset < oneSecond)
        z[] <- j0 + oneSecond
      else{
        z[] <- round((offset*j1 + (1 - offset)*j0)*86400)/86400
      }
    }
  }
  return(as.jul(z))
}

jul.Date <- function(x, ...)
  as.jul(unclass(as.vector(x + 2440588)))

jul.default <- function(x, ...){
  if(missing(x))       return(jul.Date(Sys.Date()))
  if(is.character(x))  return(jul.Date(as.Date(x, ...)))
  if(couldBeTi(x))     return(jul(as.ti(x), ...))
  if(is.ymd(x)){
    seconds <- round(86400*(unclass(x) %% 1))
    return(as.jul(ymd2jul(x) + seconds/86400))
  }
  if(is.time(x))        return(as.jul(time2jul(x)))
  else                 return(jul.Date(as.Date(x, ...)))
}

as.character.jul <- function (x, ...) format(x, ...)

c.jul <- function(..., recursive = F)
  structure(c(unlist(lapply(list(...), unclass))), class = "jul")

diff.jul <- function(x, ...) diff(stripClass(x, "jul"), ...)

hms <- function(x){
  seconds <- round((unclass(jul(x)) %% 1)*86400)
  hours   <- seconds %/% 3600
  seconds <- seconds %% 3600
  minutes <- seconds %/% 60
  seconds <- seconds %% 60
  list(hours = hours, minutes = minutes, seconds = seconds)
}

min.jul <- function(..., na.rm = F)
  structure(min(unlist(lapply(list(...), unclass)), na.rm = na.rm), class = "jul")

max.jul <- function(..., na.rm = F)
  structure(max(unlist(lapply(list(...), unclass)), na.rm = na.rm), class = "jul")

format.jul <- function(x, ...) format(as.POSIXlt(x), ...)

print.jul <- function(x, ...){
  ymds <- as.character(ymd(floor(x)))
  hmsList <- hms(x)
  if(sum(unlist(hmsList)) > 0){
    ymds <- paste(ymds,
                  substr(format(100 + hmsList$hours), 2, 3),
                  substr(format(100 + hmsList$minutes), 2, 3),
                  substr(format(100 + hmsList$seconds), 2, 3),
                  sep = ":")
  }
  names(ymds) <- names(x)
  print(ymds, quote = F, ...)
  cat("class: jul\n")
}

rep.jul <- function(x, times, ...) as.jul(NextMethod())

seq.jul <- function(...) as.jul(NextMethod())

time.jul <- function(x, offset = 1, ...){
  if(!between(offset, 0, 1)) stop("offset must be in [0,1]")
  if(offset == 1) return(jul2time(x))
  if(offset == 0) return(jul2time(x - 1))
  else {
    t0 <- jul2time(x - 1)
    t1 <- jul2time(x)
    return(t1 * offset + (1 - offset)*t0)
  }
}

"[.jul" <- function(x, ...) as.jul(NextMethod())

Ops.jul <- function (e1, e2){
  if(nargs() == 1){
    if(.Generic == "+") return(e1)
    else stop("operation not defined for jul")
  }
  isJul1 <- is.jul(e1)
  isJul2 <- is.jul(e2)
  if(isJul1 && isJul2){
    validOp <- switch(.Generic, "-" =, "<" = , ">" =, "==" =,
                      "!=" =, "<=" =, ">=" = TRUE, FALSE)
    if(validOp)
      return(do.call(.Generic, list(unclass(e1), unclass(e2))))
    else stop("operation not defined for two juls")
  }
  else {
    if(isJul1) z <- do.call(.Generic, list(unclass(e1), e2))
    else       z <- do.call(.Generic, list(e1, unclass(e2)))
    if(is.numeric(z)) class(z) <- "jul"
    return(z)
  }
}

## 'constructors' for ymd's.  Actually, since there isn't a ymd class, it
## doesn't have constructors.  But you can act as if it did. Or something....
ymd         <- function(x, ...) UseMethod("ymd")
ymd.jul     <- function(x, ...) jul2ymd(x)
ymd.ssDate  <- function(x, ...) ymd(jul(x))

ymd.ti      <- function(x, offset = 1, ...){
  if(!between(offset, 0, 1)) stop("offset must be in [0,1]")
  if(offset == 1)  ti2ymd(x)
  else             ymd(jul(x, offset))
}

ymd.default <- function(x, ...){
  if(missing(x)) jul <- jul()
  else           jul <- jul(x, ...)
  return(jul2ymd(jul))
}

## ssDate (spreadsheet date) class
is.ssDate <- function(x) inherits(x, "ssDate")
as.ssDate <- function(x) structure(x, class = "ssDate")
ssDate <- function(x, ...){
  if(missing(x)) return(ssDate(jul()))
  as.ssDate(jul(x, ...) - jul(18991230))
}
c.ssDate <- function(..., recursive = F)
  structure(c(unlist(lapply(list(...), unclass))), class = "ssDate")

min.ssDate <- function(..., na.rm = F)
  structure(min(unlist(lapply(list(...), unclass)), na.rm = na.rm), class = "ssDate")

max.ssDate <- function(..., na.rm = F)
  structure(max(unlist(lapply(list(...), unclass)), na.rm = na.rm), class = "ssDate")

print.ssDate <- function(x, ...){
  print(unclass(x), ...)
  cat("class: ssDate\n")
}

rep.ssDate <- function(x, times, ...) as.ssDate(NextMethod())
seq.ssDate <- function(...) as.ssDate(NextMethod())
time.ssDate <- function(x, offset = 1, ...) time(jul(x), offset)
"[.ssDate" <- function(x, ...) as.ssDate(NextMethod())
Ops.ssDate <- function(e1, e2){
  if(nargs() == 1){
    if(.Generic == "+") return(e1)
    else stop("operation not defined for ssDate")
  }
  is.ssDate1 <- is.ssDate(e1)
  is.ssDate2 <- is.ssDate(e2)
  if(is.ssDate1 && is.ssDate2){
    validOp <- switch(.Generic, "-" =, "<" = , ">" =, "==" =,
                      "!=" =, "<=" =, ">=" = TRUE, FALSE)
    if(validOp)
      return(do.call(.Generic, list(unclass(e1), unclass(e2))))
    else stop("operation not defined for two ssDates")
  }
  else {
    if(is.ssDate1) z <- do.call(.Generic, list(unclass(e1), e2))
    else           z <- do.call(.Generic, list(e1, unclass(e2)))
    if(is.numeric(z)) class(z) <- "ssDate"
    return(z)
  }
}

## ti class
is.ti <- function(x) inherits(x, "ti")

ti <- function(x, ...) UseMethod("ti")
ti.jul <- function(x, tif = NULL, freq = NULL,
                   hour = 0, minute = 0, second = 0, ...){
  if(is.null(tif)) tif <- freq2tif(freq)
  if(isIntradayTif(tif)){
    if(!(missing(hour) && missing(minute) && missing(second)))
      x <- floor(x + .5/86400) + (3600*hour + 60*minute + second)/86400
  }
  return(jul2ti(x, tif))
}

ti.ssDate <- function(x, ...) ti(jul(x), ...)

ti.ti <- function(x, tif = NULL, freq = NULL, ...){
  if(is.null(tif)){
    if(is.null(freq)) return(x)
    else tif <- freq2tif(freq)
  }
  return(ti(jul(x), tif, ...))
}

ti.Date <- function(x, ...) ti(jul(x), ...)

ti.default <- function(x, tif = NULL, freq = NULL, ...){
  if(is.null(tif))            tif <- freq2tif(freq)
  if(missing(x))              return(ti(jul(Sys.Date()), tif, ...))
  if(is.null(x))              stop("NULL argument to ti function")
  if(is.character(x))         return(ti(as.Date(x)), tif, ...)
  if(couldBeTi(x, tif = tif)) return(ti(as.ti(x), tif = tif, ...))
  
  if(is.ymd(x)){
    if(isIntradayTif(tif)) return(ti(as.jul(ymd2jul(x)), tif, ...))
    else                   return(ymd2ti(round(x), tif))
  }
  if(is.time(x)){
    if(isIntradayTif(tif)) return(ti(as.jul(time2jul(x)), tif, ...))
    else                   return(ti(as.jul(round(time2jul(x))), tif))
  }
  if(length(x) == 2 && is.time(x[1]) && between(x[2],1,1e8 - 1))
    return(yp2ti(1e8*x[1] + x[2], tif))
  else return(ti(as.Date(x), tif, ...))
}

couldBeTi <- function(x, tif = NULL){
  perhaps <- is.numeric(x) && (all(is.finite(x)) & between(x, 1e13, 5e13))
  if(!perhaps) return(FALSE)
  if(is.null(tif)) return(TRUE)
  else {
    nTif <- tif(tif)
    return(all(between(x, 1e10*nTif, 1e10*(nTif + 1))))
  }
}

period <- function(x){
  if(is.ti(x) || couldBeTi(x))
    stripClass(x, "ti") %% 1e10
  else NULL
}

as.character.ti <- function (x, ...) format(x, ...)

c.ti <- function(..., recursive = F)
  structure(c(unlist(lapply(list(...), unclass))), class = "ti")

min.ti <- function(..., na.rm = F)
  structure(min(unlist(lapply(list(...), unclass)), na.rm = na.rm), class = "ti")

max.ti <- function(..., na.rm = F)
  structure(max(unlist(lapply(list(...), unclass)), na.rm = na.rm), class = "ti")

diff.ti <- function(x, ...) diff(stripClass(x, "ti"), ...)

cycle.ti <- function(x, ...) ti2yp(x) %% 1e8
frequency.ti <- function(x, ...){
  f <- round(200/(time(x+100) - time(x-100)))
  if(any(index <- f > 100)) ## handle leap years 
    f[index] <- round(2000/(time(x[index]+1000) - time(x[index]-1000)))
  return(f)
}

deltat.ti <- function(x, ...) 1/frequency(x)

format.ti <- function(x, ..., tz = "GMT"){
  z <- stripClass(x, "ti")
  intraday <- isIntradayTif(tif(x))
  if(any(intraday))
    z[intraday] <- format(as.POSIXct(x[intraday]), tz = tz, ...)
  if(any(!intraday))
    z[!intraday] <- format(as.POSIXlt(x[!intraday]), ...)
  z
}

print.ti <- function(x, ...){
  j <- jul(x)
  ymds <- as.character(ymd(floor(j)))
  intraday <- isIntradayTif(tif(x))
  if(any(intraday)){
    hmsList <- hms(j[intraday])
    ymds[intraday] <- paste(ymds[intraday],
                            substr(format(100 + hmsList$hours), 2, 3),
                            substr(format(100 + hmsList$minutes), 2, 3),
                            substr(format(100 + hmsList$seconds), 2, 3),
                            sep = ":")
  }
  names(ymds) <- names(x)
  print(ymds, quote = F, ...)
  cat("class: ti\n")
}

rep.ti <- function(x, times, ...) as.ti(NextMethod())

seq.ti <- function(...) as.ti(NextMethod())

time.ti <- function(x, offset = 1, ...){
  if(!between(offset, 0, 1)) stop("offset must be in [0,1]")
  if(offset == 1) return(ti2time(x))
  if(offset == 0) return(ti2time(x - 1))
  else {
    t0 <- ti2time(x - 1)
    t1 <- ti2time(x)
    return(t1 * offset + (1 - offset)*t0)
  }
}

"[.ti" <- function(x, ...) as.ti(NextMethod())

Ops.ti <- function (e1, e2){
  if(nargs() == 1){
    if(.Generic == "+") return(e1)
    else stop("operation not defined for ti")
  }
  isTi1 <- is.ti(e1)
  isTi2 <- is.ti(e2)
  if(isTi1 && isTi2){
    n <- max(length(e1), length(e2))
    tif1 <- rep(tif(e1), length = n)
    tif2 <- rep(tif(e2), length = n)
    if(any(tif1 != tif2) && .Generic != "==") stop("different tif\'s")
    validOp <- switch(.Generic, "-" =, "<" = , ">" =, "==" =,
                      "!=" =, "<=" =, ">=" = TRUE, FALSE)
    if(validOp)
      return(do.call(.Generic, list(unclass(e1), unclass(e2))))
    else
      stop("operation not defined for two ti\'s")
  }
  else {
    if(isTi1) ti <- do.call(.Generic, list(unclass(e1), e2))
    else      ti <- do.call(.Generic, list(e1, unclass(e2)))
    class(ti) <- "ti"
    return(ti)
  }
}

## compatibility with R date classes
format.POSIXlt <- function (x, format = "", usetz = FALSE, ...){
  ## FRB added formats:
  ##    "%N"    first letter of month name
  ##    "%q"    quarter number
  if(!inherits(x, "POSIXlt"))  stop("wrong class")
  if(format == ""){
    times <- unlist(unclass(x)[1:3])
    format <- if(all(times[!is.na(times)] == 0)) "%Y-%m-%d"
    else "%Y-%m-%d %H:%M:%S"
  }
  format <- gsub("%N", "@N", gsub("%q", "@q", format))
  tmp <- .Internal(format.POSIXlt(x, format, usetz))
  
  if(length(grep("@q", format))){
    qtr <- (as.numeric(.Internal(format.POSIXlt(x, "%m", usetz))) + 2) %/% 3
    for(i in 1:length(tmp))
    tmp[i] <- gsub("@q", qtr[i], tmp[i])
  }
  
  if(length(grep("@N", format))){
    month <- as.numeric(.Internal(format.POSIXlt(x, "%m", usetz)))
    for(i in 1:length(tmp))
      tmp[i] <- gsub("@N", substring(month.abb[month[i]], 1, 1), tmp[i])
  }
  tmp
}

format.POSIXct <- function (x, format = "", tz = "", usetz = FALSE, ...){
  if(!inherits(x, "POSIXct")) stop("wrong class")
  if(missing(tz) && !is.null(tzone <- attr(x, "tzone"))) tz <- tzone
  structure(format(as.POSIXlt(x, tz), format, usetz, ...),
            names = names(x))
}

weekdays.default <- function(x, ...) weekdays(as.Date(x), ...)
months.default   <- function(x, ...) months(as.Date(x), ...)
quarters.default <- function(x, ...) quarters(as.Date(x), ...)

as.Date.jul        <- function(x, ...)     structure(x - 2440588, class = "Date")
as.Date.ti         <- function(x, ...)     as.Date(jul(x), ...)
as.POSIXct.jul     <- function(x, ...)     structure(round(unclass(x - 2440588)*86400), class = c("POSIXt", "POSIXct"))
as.POSIXct.ti      <- function(x, ...)     as.POSIXct(jul(x), ...)
as.POSIXlt         <- function(x, tz = "", ...) UseMethod("as.POSIXlt")
as.POSIXlt.POSIXlt <- function(x, tz, ...)      x
as.POSIXlt.Date    <- function(x, ...)     .Internal(Date2POSIXlt(x))
as.POSIXlt.jul     <- function(x, ...)     as.POSIXlt(as.Date(x, ...))
as.POSIXlt.ti      <- function(x, ...)     as.POSIXlt(jul(x), ...)

as.POSIXlt.default <- function(x, tz = "", ...){
  fromchar <- function(x){
    xx <- x[1]
    if(is.na(xx)){
      j <- 1
      while (is.na(xx) && (j <- j + 1) <= length(x)) xx <- x[j]
      if (is.na(xx)) 
        f <- "%Y-%m-%d"
    }
    if(is.na(xx) ||
       !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M:%S")) || 
       !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M:%S")) || 
       !is.na(strptime(xx, f <- "%Y-%m-%d %H:%M")) ||
       !is.na(strptime(xx, f <- "%Y/%m/%d %H:%M")) ||
       !is.na(strptime(xx, f <- "%Y-%m-%d")) || 
       !is.na(strptime(xx, f <- "%Y/%m/%d"))){
      res <- strptime(x, f)
      if(nchar(tz)) 
        attr(res, "tzone") <- tz
      return(res)
    }
    stop("character string is not in a standard unambiguous format")
  }
  tzone <- attr(x, "tzone")
  if(inherits(x, "date") || inherits(x, "dates")) 
    x <- as.POSIXct(x)
  if(is.character(x)) 
    return(fromchar(x))
  if(is.factor(x)) 
    return(fromchar(as.character(x)))
  if(is.logical(x) && all(is.na(x))) 
    x <- as.POSIXct.default(x)
  if(!inherits(x, "POSIXct")) 
    stop(paste("Don't know how to convert `", deparse(substitute(x)), 
               "' to class \"POSIXlt\"", sep = ""))
  if(missing(tz) && !is.null(tzone)) 
    tz <- tzone[1]
  .Internal(as.POSIXlt(x, tz))
}

## workhorse functions that convert ti's back and forth to ymd and jul
julToTi <- function(jul, tif, must.handle=F){
  nTif <- tif(tif)
  j <- unclass(jul)
  if(between(nTif, 1001, 1009) || between(nTif, 1011, 1025)){
    period <- switch(nTif - 1e3,  ## handle day-based freqs
                     ## 1 = daily
                     j - 2415019,
                     ## 2 = business day
                     { 
                       dow <- julToWeekday(j)
                       j <- j + (dow==1) + 2*(dow==7)
                       ((j - 2415021)%/%7)*5 + (j - 2415020)%%7
                     },
                     ## 3 - 9 = weeklySunday thru weeklySaturday
                     (j - 2415007)%/%7,
                     (j - 2415008)%/%7,
                     (j - 2415009)%/%7,
                     (j - 2415010)%/%7,
                     (j - 2415011)%/%7,
                     (j - 2415012)%/%7,
                     (j - 2415013)%/%7,
                     ## 10 is tenday, diverted by the if statement above 
                     NULL,
                     ## 11 (reserves) is weekly Wednesday until 19840201 and
                     ## biweekly Wednesday afterwards. firstCrrDay is 19840202.
                     {
                        firstCrrDay <- 2445733
                        preCrr <- j < firstCrrDay
                        prePeriods  <-  (j - 2415010)%/%7
                        postPeriods <- 4389 + (j - firstCrrDay)%/%14
                        prePeriods*preCrr + postPeriods*(!preCrr)
                     },
                     ## 12 - 25 = biweekly1Sunday thru biweekly2Saturday
                     (j - 2415000)%/%14,
                     (j - 2415001)%/%14,
                     (j - 2415002)%/%14,
                     (j - 2415003)%/%14,
                     (j - 2415004)%/%14,
                     (j - 2415005)%/%14,
                     (j - 2415006)%/%14,
                     (j - 2415007)%/%14,
                     (j - 2415008)%/%14,
                     (j - 2415009)%/%14,
                     (j - 2415010)%/%14,
                     (j - 2415011)%/%14,
                     (j - 2415012)%/%14,
                     (j - 2415013)%/%14)
    ans <- as.ti(1e10*nTif + period)
    names(ans) <- names(jul)
    return(ans)
  }
  if(isIntradayTif(nTif)){
    j19800101 <- 2444240    ## julian date for Jan 1, 1980
    if(any(j < j19800101)) stop("Intraday ti cannot be earlier than 19800101")
    hms <- nTif %/% 1e3
    nUnits <- nTif %% 1e3
    if(nUnits == 0) nUnits <- 1
    period <- switch(hms - 1, ## 1 = hourly, 2 = minutely, 3 = secondly
                     1 + round((j - j19800101)*(24/nUnits)),
                     1 + round((j - j19800101)*(1440/nUnits)),
                     1 + round((j - j19800101)*(86400/nUnits)))
    ans <- as.ti(1e10*nTif + period)
    names(ans) <- names(jul)
    return(ans)
  }  
  else { ## call ymdToTi for other frequencies
    if(must.handle)
      stop(paste(tif, "is an unknown frequency"))
    else 
      return(do.call("ymdToTi",
                     list(ymd = jul2ymd(j), tif = tif, must.handle=T)))
    
  }
}

ymdToTi <- function(ymd, tif, must.handle=F){
  rawYmd <- unclass(ymd)
  year   <- rawYmd %/% 10000
  month  <- (rawYmd - year*10000) %/% 100
  day    <- rawYmd %% 100
  
  nTif <- tif(tif)
  if(nTif == 1010) ## tenday
    return(1 + 36*(year-1900) + 3*(month-1) + (day>10) + (day>20))
  if(between(nTif, 1026, 1050)){
    period <- switch(nTif - 1e3 - 25,
                     ## 26 = twicemonthly
                     1 + 24*(year-1900) + 2*(month-1) + (day>14),
                     ## 27 = monthly
                     12*(year-1800) + month,
                     ## 28 and 29 are bimonthly (nov and dec)
                     6*(year-1800) + (month+1) %/% 2, 
                     6*(year-1800) + (month+1) %/% 2,
                     ## 30 - 32 are quarterly (oct, nov and dec)
                     4*(year-1800) + (month+2) %/% 3, 
                     4*(year-1800) + (month+2) %/% 3, 
                     4*(year-1800) + (month+2) %/% 3,
                     ## 33 - 44 are annual (jan, feb, ..., dec)
                     1 + (year-1600) + (month > 1),
                     1 + (year-1600) + (month > 2),
                     1 + (year-1600) + (month > 3),
                     1 + (year-1600) + (month > 4),
                     1 + (year-1600) + (month > 5),
                     1 + (year-1600) + (month > 6),
                     1 + (year-1600) + (month > 7),
                     1 + (year-1600) + (month > 8),
                     1 + (year-1600) + (month > 9),
                     1 + (year-1600) + (month > 10),
                     1 + (year-1600) + (month > 11),
                     1 + (year-1600),
                     ## 45 - 50 are semi-annual (jul, aug, ..., dec)
                     1 + 2*(year-1600) + (month > 6) + ((month %% 6) > 1),
                     1 + 2*(year-1600) + (month > 6) + ((month %% 6) > 2),
                     1 + 2*(year-1600) + (month > 6) + ((month %% 6) > 3),
                     1 + 2*(year-1600) + (month > 6) + ((month %% 6) > 4),
                     1 + 2*(year-1600) + (month > 6) + ((month %% 6) > 5),
                     1 + 2*(year-1600) + (month > 6))
    ans <- as.ti(1e10*nTif + period)
    names(ans) <- names(ymd)
    return(ans)
  }
  else { ## call ti.jul for other frequencies
    if(must.handle)
      stop(paste(tif, "is an unknown frequency"))
    else 
      return(do.call("julToTi",
                     list(jul = ymd2jul(ymd),tif = tif, must.handle=T)))
  }
}

tiToJul <- function(ti, must.handle=F){
  uti <- as.vector(unclass(ti))
  nTif <- uti[1] %/% 1e10
  periods <- (uti %% 1e10) - 1
  
  if(between(nTif, 1001, 1009) || between(nTif, 1011, 1025)){
    j <- switch(nTif - 1e3,  ## handle day-based freqs
                ## 1 = daily
                2415020 + periods,
                ## 2 = business day
                2415021 + 7*(periods %/% 5) + periods %% 5,
                ## 3 - 9 = weeklySunday thru weeklySaturday
                2415020 + periods*7,
                2415021 + periods*7,
                2415022 + periods*7,
                2415023 + periods*7,
                2415024 + periods*7,
                2415025 + periods*7,
                2415026 + periods*7,
                ## Use placeholder for 10 (= tenday)
                NULL,
                ## need code for 11 (= reserves)
                {
                  preCrr  <- periods < 4388
                  preJul  <- 2415023 + periods*7
                  postJul <- 2445732 + (periods - 4387)*14
                  preJul*preCrr + postJul*(!preCrr)
                },
                ## 12 - 25 = biweekly1Sunday thru biweekly2Saturday
                2415027 + periods*14,
                2415028 + periods*14,
                2415029 + periods*14,
                2415030 + periods*14,
                2415031 + periods*14,
                2415032 + periods*14,
                2415033 + periods*14,
                2415034 + periods*14,
                2415035 + periods*14,
                2415036 + periods*14,
                2415037 + periods*14,
                2415038 + periods*14,
                2415039 + periods*14,
                2415040 + periods*14)
    names(j) <- names(ti)
    return(as.jul(j))
  }
  else{
    if(isIntradayTif(nTif)){
      j19800101 <- 2444240    ## julian date for Jan 1, 1980
      hms <- nTif %/% 1e3
      nUnits <- nTif %% 1e3
      if(nUnits == 0) nUnits <- 1
      j <- switch(hms - 1, ## 1 = hourly, 2 = minutely, 3 = secondly
                  j19800101 + (nUnits*periods)/24,
                  j19800101 + (nUnits*periods)/1440,
                  j19800101 + (nUnits*periods)/86400)
      names(j) <- names(ti)
      return(as.jul(j))
    }
    else { ## pass other frequencies to tiToYmd
      if(must.handle)  stop(paste(nTif, "is an unknown frequency"))
      else
        return(ymd2jul(tiToYmd(ti, must.handle=T)))
    }
  }
}

tiToYmd <- function(ti, must.handle=F){
  uti <- as.vector(unclass(ti))
  nTif <- uti[1] %/% 1e10
  periods <- (uti %% 1e10) - 1
  mdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(nTif == 10){ ## tenday
    y <- 1900 + periods %/% 36
    m <- 1 + (periods %% 36)/3
    dmat <- cbind(rep(10,12), rep(20,12), mdays)
    d <- dmat[m, 1 + periods%%3]
  }
  else {
    if(between(nTif, 1026, 1050)){
      switch(nTif - 1e3 - 25, 
             ## 26 = twicemonthly
             { y <- 1900 + periods %/% 2
               m <- 1 + (periods %% 24) %/% 2
               half <- periods %% 2
               d <- 15*(half==0) + mdays[m]*(half==1)
             },
             ## 27 = monthly
             { y <- 1800 + periods %/% 12
               m <- 1 + periods %% 12
               d <- mdays[m]
             },
             ## 28 and 29 are bimonthly (nov and dec)
             { y <- 1800 + periods %/% 6
               m <- 1 + 2*(periods %% 6)
               d <- mdays[m]
             }, 
             { y <- 1800 + periods %/% 6
               m <- 2 + 2*(periods %% 6)
               d <- mdays[m]
             },
             ## 30 - 32 are quarterly (oct, nov and dec)
             { y <- 1800 + periods %/% 4
               m <- 1 + 3*(periods %% 4)
               d <- mdays[m]
             },
             { y <- 1800 + periods %/% 4
               m <- 2 + 3*(periods %% 4)
               d <- mdays[m]
             }, 
             { y <- 1800 + periods %/% 4
               m <- 3 + 3*(periods %% 4)
               d <- mdays[m]
             },
             ## 33 - 44 are annual (jan, feb, ..., dec)
             { y <- 1600 + periods
               m <- 1
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 2
               d <- mdays[m]
             },
             { y <- 1600 + periods
               m <- 3
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 4
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 5
               d <- mdays[m]
             },
             { y <- 1600 + periods
               m <- 6
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 7
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 8
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 9
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 10
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 11
               d <- mdays[m]
             }, 
             { y <- 1600 + periods
               m <- 12
               d <- mdays[m]
             },
             ## 45 - 50 are semi-annual (jul, aug, ..., dec)
             { y <- 1600 + periods %/% 2
               m <- 1 + 6*(periods %% 2)
               d <- mdays[m]
             }, 
             { y <- 1600 + periods %/% 2
               m <- 2 + 6*(periods %% 2)
               d <- mdays[m]
             }, 
             { y <- 1600 + periods %/% 2
               m <- 3 + 6*(periods %% 2)
               d <- mdays[m]
             }, 
             { y <- 1600 + periods %/% 2
               m <- 4 + 6*(periods %% 2)
               d <- mdays[m]
             }, 
             { y <- 1600 + periods %/% 2
               m <- 5 + 6*(periods %% 2)
               d <- mdays[m]
             }, 
             { y <- 1600 + periods %/% 2
               m <- 6 + 6*(periods %% 2)
               d <- mdays[m]
             })
    }
    else { ## pass other frequencies to tiToJul
      if(must.handle)
        stop(paste(tif, "is an unknown frequency"))
      else
        return(jul2ymd(tiToJul(ti, must.handle=T)))
    }
  }
  d <- d + (isLeapYear(y) & m == 2)
  ymd <- y*10000 + m*100 + d
  names(ymd) <- names(ti)
  return(ymd)
}

tif <- function(x, ...) UseMethod("tif")
tif.ti  <- function(x, ...) unclass(x) %/% 1e10
tif.tis <- function(x, ...) tif(start(x))
tif.ts  <- function(x, ...) tif(as.tis(x))
tif.default <- function(x, freq = NULL, ...){
  ## freq ignored unless missing(x), ... is always ignored
  if(missing(x)){
    if(is.null(freq)) return(tifList())
    else return(freq2tif(freq))
  }
  if(is.numeric(x) && between(x, 1001, 4900))
    return(as.vector(unclass(x)))
  else {
    if(is.character(x)){
      xlen <- length(x)
      ans <- numeric(xlen) + NA
      intraday <- substr(x, 1, 6) %in% c("hourly", "minute", "second")
      if(any(intraday)){
        spots <- (1:xlen)[intraday]
        for(spot in spots) ans[spot] <- eval(parse(text = x[spot]))
      }
      if(any(!intraday))
        ans[!intraday] <- as.vector(tifList()[x[!intraday]])
      if(any(is.na(ans))) stop("no such tif")
      else return(ans)
    }
    else stop("arg must be ti, tis, or ts")
  }
}

## Intraday frequencies
isIntradayTif <- function(tif){
  if(is.character(tif)) tif <- tif(tif)
  z <- between(tif, 2000, 4900)
  z[is.na(z)] <- F
  z
}
hourly <- function(n = 0){
  if(n == 0) return(2000)
  if(!between(n, 0, 24)) stop("n out of range")
  if(!(24 %% n == 0)) stop("n not a factor of 24")
  return(2000 + n)
}
minutely <- function(n = 0){
  if(n == 0) return(3000)
  if(!between(n, 0, 1024)) stop("n out of range")
  if((n %% 60) == 0) return(hourly(n %/% 60))
  if(!(1440 %% n == 0)) stop("n not a factor of 1440")
  return(3000 + n)
}
secondly <- function(n = 0){
  if(n == 0) return(4000)
  if(!between(n, 0, 1024)) stop("n out of range")
  if((n %% 60) == 0) return(minutely(n %/% 60))
  if(!(86400 %% n == 0)) stop("n not a factor of 86400")
  return(4000 + n)
}

## Support functions
isLeapYear    <- function(y) y %% 4 == 0 & (y %% 100 != 0 | y %% 400 == 0)
is.ymd        <- function(x) all(between(x, 17990101, 21991231))
is.time       <- function(x) all(between(x, 1799, 2200))
julToWeekday  <- function(jul){
  ## Sun = 1, Sat = 7.  2415020 = Sunday, 12/31/1899
  ((unclass(jul) - 2415020) %% 7) + 1
}

freq2tif <- function(freq){
  if(is.null(freq)) stop("NULL freq")
  if(!is.numeric(freq)) stop("freq must be numeric")
  tif <- switch((freq >= 1) + (freq >= 2) + (freq >= 4) + 
                (freq >= 6) + (freq >= 12) + (freq >= 24) + 
                (freq >= 26) + (freq >= 36) + (freq >= 52) + 
                (freq >= 262) + (freq >= 365), 
                44, 50, 32, 29, 27, 26, 22, 10, 4, 2, 1)
  if(is.null(tif)) stop("can't convert freq to tif")
  else             return(1e3 + tif)
}

tif2freq <- function(tif)  frequency(ti(tif = tif))

tifName <- function(s) UseMethod("tifName")
tifName.tis <- function(s) tifName(start(s))
tifName.ti <- function(s){
  nTif <- tif(s)
  cTif <- stripClass(s, "ti")
  mode(cTif) <- "character"
  intraday <- isIntradayTif(nTif)
  if(any(intraday)){
    base <- c("hourly", "minutely", "secondly")[(nTif[intraday] %/% 1000) -1]
    nUnits <- as.character(nTif[intraday] %% 1000)
    nUnits[nUnits == "0"] <- ""
    cTif[intraday] <- paste(base, "(", nUnits, ")", sep = "")
  }
  if(any(notIntraday <- !intraday)){
    tl <- tifList()
    cTif[notIntraday] <- names(tl)[match(nTif[notIntraday], tl)]
  }
  cTif
}

tifName.default <- function(s){
  tl <- tifList()
  if(missing(s)) return(tl)
  if(is.character(s)){
    if(all((s %in% names(tl)) |
           (substr(s, 1, 6) %in% c("hourly", "minute", "second"))))
      return(s)
    else stop("unknown tifName")
  }
  else{
    if(!is.numeric(s)) stop("non-numeric, non-character arg s")
    cTif <- character(length(s))
    intraday <- isIntradayTif(s)
    if(any(intraday)){
      base <- c("hourly", "minutely", "secondly")[(s[intraday] %/% 1000) - 1]
      nUnits <- as.character(s[intraday] %% 1000)
      nUnits[nUnits == "0"] <- ""
      cTif[intraday] <- paste(base, "(", nUnits, ")", sep = "")
    }
    if(any(notIntraday <- !intraday))
      cTif[notIntraday] <- names(tl)[match(s[notIntraday], tl)]
    return(cTif)
  }
}

initialTifList <- function(){
  c(daily               = 1001,
    business            = 1002,
    wsunday             = 1003,
    wmonday             = 1004,
    wtuesday            = 1005,
    wwednesday          = 1006,
    wthursday           = 1007,
    wfriday             = 1008,
    wsaturday           = 1009,
    tenday              = 1010,
    reserves            = 1011,
    bw1sunday           = 1012,
    bw1monday           = 1013,
    bw1tuesday          = 1014,
    bw1wednesday        = 1015,
    bw1thursday         = 1016,
    bw1friday           = 1017,
    bw1saturday         = 1018,
    bw2sunday           = 1019,
    bw2monday           = 1020,
    bw2tuesday          = 1021,
    bw2wednesday        = 1022,
    bw2thursday         = 1023,
    bw2friday           = 1024,
    bw2saturday         = 1025,
    twicemonth          = 1026,
    semimonthly         = 1026,
    monthly             = 1027,
    bimonth1            = 1028,
    bmnovember          = 1028,
    bimonthnovember     = 1028,
    bimonth2            = 1029,
    bmdecember          = 1029,
    bimonthdecember     = 1029, 
    bimonth             = 1029,
    quarterlyoctoctober = 1030,
    qoctober            = 1030,
    quarterlynovember   = 1031,
    qnovember           = 1031,
    quarterlydecember   = 1032,
    qdecember           = 1032,
    annjanuary          = 1033,
    annfebruary         = 1034,
    annmarch            = 1035,
    annapril            = 1036,
    annmay              = 1037,
    annjune             = 1038,
    annjuly             = 1039,
    annaugust           = 1040,
    annseptember        = 1041,
    annoctober          = 1042,
    annnovember         = 1043,
    anndecember         = 1044,
    sannjuly            = 1045,
    sannaugust          = 1046,
    sannseptember       = 1047,
    sannoctober         = 1048,
    sannnovember        = 1049,
    sanndecember        = 1050,
    unknown             =    0)
}

tifList <- function(){
  if(!exists(".tifList", env = globalenv()))
    tl <- setDefaultFrequencies(setup = TRUE)
  else{
    tl <- get(".tifList", env = globalenv())
    if(is.na(tl["weekly"]))
      tl <- setDefaultFrequencies(setup = FALSE)
  }
  return(tl)
}

setDefaultFrequencies <- function(weekly     = "wmonday",
                                  biweekly   = "bw2wednesday",
                                  bimonthly  = "bimonthdecember",
                                  quarterly  = "qdecember",
                                  annual     = "anndecember",
                                  semiannual = "sanndecember",
                                  setup = FALSE){
  if(setup) tl <- initialTifList()
  else {
    if(!exists(".tifList", env = globalenv()))
      assign(".tifList", initialTifList(), env = globalenv())
    tl <- get(".tifList", env = globalenv())
  }
  if(!missing(weekly) || setup)
    tl["weekly"] <- tl[weekly]
  if(!missing(biweekly) || setup)
    tl["biweekly"] <- tl[biweekly]
  if(!missing(bimonthly) || setup)
    tl[c("bimonth", "bimonthly")] <- tl[bimonthly]
  if(!missing(quarterly) || setup)
    tl[c("quarterly", "q")] <- tl[quarterly]
  if(!missing(annual) || setup)
    tl[c("annual", "a")] <- tl[annual]
  if(!missing(semiannual) || setup)
    tl[c("sann", "semiannual")]  <- tl[semiannual]
  assign(".tifList", tl, env = globalenv())
}
