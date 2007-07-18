## Time Indexed Series
is.tis <- function(x) inherits(x, "tis")

tis <- function(data, start = 1, tif = NULL, frequency = NULL, 
                end = NULL){
  n <- NROW(data)
  if(missing(start)){
    if(missing(end)) stop("start or end must be given \n")
    else {
      endTi   <- ti(end, tif = tif, freq = frequency)
      startTi <- endTi - n + 1
    }
  }
  else {
    startTi <- ti(start, tif = tif, freq = frequency)
    if(missing(end))
      endTi <- startTi + n - 1
    else {
      endTi <- ti(end, tif = tif, freq = frequency)
      n <- endTi + 1 - startTi
    }
  }
  
  x <- unclass(data)
  if(NROW(x) != n){
    if(is.matrix(x)) x <- apply(x, 2, rep, length.out = n)
    else x <- rep(x, length.out = n)
  }
  if(!is.null(dnx <- dimnames(x))) 
    dimnames(x) <- list(character(0), dnx[[2]])
  start(x) <- startTi
  class(x) <- "tis"
  x
}

ti.tis <- function(x, ...) start(x) + 0:(NROW(x)-1)

tif.tis <- function(x, ...) tif(start(x), ...)

start.tis <- function(x, ...) attr(x, "start")

"start<-" <- function(x, value){
  attr(x, "start") <- value
  x
}

end.tis <- function(x, ...){
  n <- NROW(x)
  if(n > 0) start(x) + n - 1
  else start(x)
}

stripTis <- function(x){
  z <- stripClass(x, "tis")
  attr(z, "start") <- NULL
  z
}

as.tis <- function(x, ...) UseMethod("as.tis")
as.tis.tis <- function(x, ...) x
as.tis.ts <- function(x, ...){
  sti <- ti(start(x), freq = frequency(x))
  tsp(x) <- NULL
  tis(x, start = sti)
}

as.tis.default <- function(x, ...){
  tis(x, ...)
}

as.ts.tis <- function(x, ...){
  xstart <- start(x)
  ts(stripTis(x), start = c(year(xstart), cycle(xstart)), frequency = frequency(x))
}

aggregate.tis <- function (x, FUN = sum, ...){
  argList <- list(...)
  if (missing(FUN) && !is.null(argList$fun)) 
    argList$FUN <- argList$fun
  else
    argList$FUN <- FUN
  argList$fun <- NULL
  argList$x <- as.ts(x)
  as.tis(do.call("aggregate", argList))
}

aggregate.ts <- function (x, nfrequency = 1, FUN = sum, ndeltat = 1,
                          ts.eps = getOption("ts.eps"),  ...) {
  ## fixed to start on nfreq boundary
  x <- as.ts(x)
  ofrequency <- tsp(x)[3]
  if (missing(nfrequency)) 
    nfrequency <- 1/ndeltat
  if ((nfrequency > 1) && (abs(nfrequency - round(nfrequency)) < 
                           ts.eps)) 
    nfrequency <- round(nfrequency)
  if (nfrequency == ofrequency) 
    return(x)
  if (abs(ofrequency%%nfrequency) > ts.eps) 
    stop(paste("cannot change frequency from", ofrequency, 
               "to", nfrequency))
  len <- ofrequency%/%nfrequency
  mat <- is.matrix(x)
  if (mat) 
    cn <- colnames(x)
  nstart <- ceiling(tsp(x)[1]*nfrequency)/nfrequency
  ## nstart <- tsp(x)[1]
  x <- as.matrix(window(x, start = nstart))
  nend <- floor(nrow(x)/len) * len
  x <- apply(array(c(x[1:nend, ]), dim = c(len, nend/len, ncol(x))), 
             MARGIN = c(2, 3), FUN = FUN, ...)
  if (!mat) 
    x <- as.vector(x)
  else colnames(x) <- cn
  ts(x, start = nstart, frequency = nfrequency)
}

cummax.tis <- function(x){
  xs <- stripTis(x)
  if(is.matrix(xs)) xs <- apply(xs, 2, cummax)
  else              xs <- cummax(xs)
  tis(xs, start = start(x))
}

cummin.tis <- function(x){
  xs <- stripTis(x)
  if(is.matrix(xs)) xs <- apply(xs, 2, cummin)
  else              xs <- cummin(xs)
  tis(xs, start = start(x))
}

cumprod.tis <- function(x){
  xs <- stripTis(x)
  if(is.matrix(xs)) xs <- apply(xs, 2, cumprod)
  else              xs <- cumprod(xs)
  tis(xs, start = start(x))
}

cumsum.tis <- function(x){
  xs <- stripTis(x)
  if(is.matrix(xs)) xs <- apply(xs, 2, cumsum)
  else              xs <- cumsum(xs)
  tis(xs, start = start(x))
}

frequency.tis <- function(x, ...) frequency(start(x))
deltat.tis <- function(x, ...) 1/frequency(x)
cycle.tis <- function(x, ...) cycle(ti(x))
time.tis <- function(x, ...)  time(ti(x), ...)

dateRange <- function(x){
  if(is.tis(x)) start(x) + c(0, NROW(x) - 1)
  else ti(start(x), freq = frequency(x)) + c(0, NROW(x) - 1)
}

lag.tis <- function(x, k = 1, ...){
  start(x) <- start(x) - round(k)
  x
}

lines.tis <- function(x, offset = 0.5, dropNA = FALSE, ...){
  xcts    <- POSIXct(ti(x), offset = offset)
  xtimes  <- time(xcts)
  xrange  <- par("usr")[1:2]
  ctSum   <- sum(between(unclass(xcts), xrange[1], xrange[2]))
  timeSum <- sum(between(xtimes, xrange[1], xrange[2]))
  xt <- if(ctSum > timeSum) xcts else xtimes
  if(dropNA){
    naSpots <- is.na(x)
    xt <- xt[!naSpots]
    x  <- x[!naSpots]
  }
  lines.default(xt, x, ...)
}

diff.tis <- function(x, lag = 1, differences = 1, ...){
  if(lag < 1 || lag != round(lag))
    stop("lag must be a positive integer")
  if(differences < 1 || differences != round(differences))
    stop("differences must be a positive integer")
  dimx <- dim(x)
  n <- if(is.null(dimx)) length(x) else dimx[1]
  j <- lag * differences
  if(j >= n) return(x[0])
  zStart <- start(x) + j
  z <- stripTis(x)
  l <- 1:lag
  m <- n - l + 1
  if(length(dimx) < 2)
    for(i in 1:differences) {
      z <- z[ - l] - z[ - m]
      m <- m - lag
    }
  else for(i in 1:differences) {
    z <- z[ - l,  , drop = F] - z[ - m,  , drop = F]
    m <- m - lag
  }
  tis(z, start = zStart)
}

window.tis <- function(x, start = NULL, end = NULL, extend = FALSE, noWarn = FALSE, ...){
  xStart <- start(x)
  xEnd   <- end(x)
  xDim   <- dim(x)
  isMat <- is.matrix(x)
  tif <- tif(x)

  ## figure yStart
  if(missing(start) || is.null(start)) 
    yStart <- xStart
  else 
    yStart <- ti(start, tif = tif)
 
  if(yStart < xStart && !extend){
    yStart <- xStart
    if(!noWarn) warning("start value of series not changed")
  }
  
  ## figure yEnd
  if(missing(end) || is.null(end)) 
    yEnd <- xEnd
  else 
    yEnd <- ti(end, tif = tif)

  if(yEnd > xEnd && !extend){
    yEnd <- xEnd
    if(!noWarn) warning("end value of series not changed")
  }

  if(yStart > yEnd) stop("start cannot be after end")

  if(!extend){
    lo.index <- 1 + yStart - xStart
    hi.index <- lo.index + yEnd - yStart
    if(isMat) 
      z <- x[lo.index:hi.index, drop=F]
    else 
      z <- x[lo.index:hi.index]
  }
  else{ ## extend 
    if(isMat) 
      z<- matrix(NA, nrow = (yEnd - yStart + 1), ncol = dim(x)[2])
    else 
      z<- rep(NA, yEnd - yStart + 1)

    if(yStart <= xStart){
      lox.index <- 1
      loz.index <- xStart - yStart+1
    }
    else{
      lox.index <- 1 + yStart - xStart
      loz.index <- 1
    }
    if (yEnd >= xEnd) 
      hix.index <- NROW(x)
    else
      hix.index <- 1 + xStart - xEnd
    hiz.index <- loz.index + (hix.index - lox.index + 1) - 1
    if(isMat)
      z[loz.index:hiz.index,]<- x[lox.index:hix.index, drop = F]
    else
      z[loz.index:hiz.index]<- x[lox.index:hix.index]
  }
  y <- tis(z, start = yStart)
  if(!is.null(xbasis <- attr(x, "basis")))
     attr(y, "basis") <- xbasis
  if(!is.null(xobserved <- attr(x, "observed")))
     attr(y, "observed") <- xobserved
  if(is.null(xDim)) dim(y) <- NULL
  class(y) <- "tis"
  y
}

as.matrix.tis <- function(x, ...){
  if(length(dim(x)) != 2)
    dim(x) <- c(length(x), 1)
  x
}

as.data.frame.tis <- function (x, ...){
  if (is.matrix(x)) 
    as.data.frame.matrix(x, ...)
  else as.data.frame.vector(x, ...)
}

Ops.tis <- function(e1, e2){ 
  if(nargs() == 1) { ## unary operators
    val <- switch(.Generic,
                  "-" = -1 * e1,
                  "+" = e1,
                  "!" = !as.logical(e1))
    return(val)
  }
  tisArg <- nchar(.Method) > 0
  if(!all(tisArg)){  ## one of e1,e2 is not a tis object
    if(tisArg[1]){ ## e2 is not tis
      if(is.ts(e2))
        e2 <- as.tis(e2)
      else return(NextMethod(.Generic))
    }
    if(tisArg[2]){ ## e1 is not tis
      if(is.ts(e1))
        e1 <- as.tis(e1)
      else return(NextMethod(.Generic))
    }
  }
  ## if we've gotten this far, e1 and e2 are both tis
  start1 <- start(e1)
  start2 <- start(e2)
  start3 <- max(start1, start2)
  end1 <- end(e1)
  end2 <- end(e2)
  end3 <- min(end1, end2)
  if(start3 > end3) stop("non-overlapping tis series")
  ## still here? Window the series and proceed
  if(!((start1 == start3)&&(end1 == end3)))
    e1 <- window(e1, start = start3, end = end3)
  if(!((start2 == start3)&&(end2 == end3)))
    e2 <- window(e2, start = start3, end = end3)
  result <- NextMethod(.Generic)
  if(!is.tis(result)) result <- tis(result, start = start3)
  return(result)
}

points.tis <- function(x, offset = 0.5, dropNA = FALSE, ...){
  xcts    <- POSIXct(ti(x), offset = offset)
  xtimes  <- time(xcts)
  xrange  <- par("usr")[1:2]
  ctSum   <- sum(between(unclass(xcts), xrange[1], xrange[2]))
  timeSum <- sum(between(xtimes, xrange[1], xrange[2]))
  xt <- if(ctSum > timeSum) xcts else xtimes
  if(dropNA){
    naSpots <- is.na(x)
    xt <- xt[!naSpots]
    x  <- x[!naSpots]
  }
  points.default(xt, x, ...)
}

print.tis <- function(x, format = "%Y%m%d", matrix.format = F, ...){
  f <- frequency(x)
  nc <- NCOL(x)
  if((nc == 1) && ((f == 4) || (f == 12)) && !matrix.format)
    print(as.ts(x), ...)
  else {
    xtif <- tif(x)
    if(missing(format) && isIntradayTif(xtif)){
      if(between(xtif, 2000, 2900)) format <- "%Y%m%d:%H"
      if(between(xtif, 3000, 3900)) format <- "%Y%m%d:%H:%M"
      if(between(xtif, 4000, 4900)) format <- "%Y%m%d:%H:%M:%S"
    }
    if(NROW(x) > 0) rNames <- format(ti(x), format = format)
    else            rNames <- character(0)
    if(is.null(cNames <- dimnames(x)[[2]]))
      cNames <- character(0)
    print(matrix(unclass(x), ncol = nc, dimnames = list(rNames, cNames)))
  }
  cat("class: tis\n")
  invisible(x)
}

t.tis <- function(x) t(stripTis(x))

cbind.tis <- function(..., union = F){
  object <- substitute(list(...))[-1]
  x <- list(...)[sapply(list(...), length) > 0]
  ## x <- list(...)
  nx <- length(x)
  if(nx < 1) stop("No data")
  tisArg <- sapply(x, is.tis)
  tisX <- x[tisArg]
  starts <- as.ti(sapply(tisX, start))
  ends   <- as.ti(sapply(tisX, end))
  tifs   <- sapply(tisX, tif)
  if(any(tifs != tifs[1]))
    stop("time series have different frequencies")
  if(union){
    start <- min(starts)
    end   <- max(ends)
  }
  else{
    start <- max(starts)
    end   <- min(ends)
  }
  if(start > end) stop("Non-intersecting series")
  for(i in seq(x)){
    if(!tisArg[i])
    x[[i]] <- tis(x[[i]], start = start, end = end)
  }
  z <- NULL
  argnames <- names(x)	## were names given?
  if(length(argnames) != nx)
    argnames <- character(nx)
  no.argname <- nchar(argnames) == 0
  argnames <- as.list(argnames)
  for(i in seq(x)){
    ser <- x[[i]]
    if(is.matrix(ser)){
      labels <- dimnames(ser)[[2]]
      ncol <- dim(ser)[2]
      cols <- 1:ncol
      if(length(labels) != ncol){
        if(no.argname[i])  argnames[[i]] <- deparse(object[[i]])
        labels <- if(ncol > 1) paste(argnames[[i]], cols, sep = ".") 
        else argnames[[i]]
      }
      else{
        if(!no.argname[i])
          labels <- paste(argnames[[i]], labels, sep = ".")
      }
      argnames[[i]] <- labels
    }
    else{
      ## Univariate case
      if(no.argname[i]) argnames[[i]] <- deparse(object[[i]])
    }
    if(union){
      start.i <- starts[i]
      end.i <- ends[i]
      if(is.matrix(ser)){
        if(start.i > start) ser[start,] <- NA
        if(end.i < end) ser[end,] <- NA
      }
      else{
        if(start.i > start) ser[start] <- NA
        if(end.i < end) ser[end] <- NA
      }
      ans <- ser
    }
    else ans <- window(ser, start = start, end = end)
    z <- cbind(z, stripTis(ans))
  }
  colnames <- unlist(argnames)
  noname <- nchar(colnames) == 0
  if(any(noname))
    colnames[noname] <- paste("Ser", 1:length(colnames), sep = ".")[noname]
  start(z) <- start
  class(z) <- "tis"
  dimnames(z) <- list(character(0), colnames)
  z
}

mergeSeries <- function(x, y, differences=F){
  ## where x and y overlap, y values are used
  ## if diff == T, the first differences are merged, and then
  ## cumulatively summed.  If start(y) <= start(x), the first
  ## obs will be from y, else it is from x.  Column names of x
  ## are updated by column names from y, if any.
  x <- as.tis(x)
  y <- as.tis(y)
  if(tif(x) != tif(y)) stop("incompatible tifs")
  xCols <- if(is.matrix(x)) dim(x)[2] else 1
  yCols <- if(is.matrix(y)) dim(y)[2] else 1
  if(xCols != yCols) stop("incompatible number of columns")

  xStart <- start(x) 
  yStart <- start(y)
  xRows  <- NROW(x)
  yRows  <- NROW(y)
  
  zStart <- min(xStart, yStart)
  zRows  <- max(xStart + xRows, yStart + yRows) - zStart 

  ix <- (1:xRows) + xStart - zStart
  iy <- (1:yRows) + yStart - zStart

  if(xCols == 1){
    z <- numeric(zRows) + NA
    if(differences){
      if(zStart == yStart) firstval <- y[1]
      else firstval <- x[1]
      za <- c(firstval, unclass(mergeSeries(diff(x), diff(y))))
      z <- cumsum(za)
    }
    else{
      z[ix] <- x[]
      z[iy] <- y[]
    }
  }
  else{
    z <- matrix(NA, zRows, xCols)
    if(differences){
      if(zStart == yStart) firstval <- y[1,]
      else firstval <- x[1,]
      za <- rbind(firstval, unclass(mergeSeries(diff(x), diff(y)))) 
      for(i in 1:cols) z[,i] <- cumsum(za[,i])
    }
    else{
      for(i in 1:xCols){
        z[ix, i] <- x[,i]
        z[iy, i] <- y[,i]
      }
    }
    xDn <- dimnames(x)
    yDn <- dimnames(y)
    zColnames <- character(0)
    if(!is.null(xDn[[2]])) zColnames <- xDn[[2]]
    if(!is.null(yDn[[2]])) zColnames <- yDn[[2]]
    dimnames(z) <- list(character(0), zColnames)
  }
  tis(z, start = zStart)
}

"[.tis" <- function(x, i, j, drop = T){
  if(is.null(dim(x))) dim(x) <- length(x)
  if(missing(i) && missing(j)) 
    return(as.vector(unclass(x)[,drop=drop]))
  if(missing(i)){
    z <- unclass(x)[, j, drop = drop]
    start(z) <- start(x)
    class(z) <- class(x)
    return(z)
  }
  tif <- tif(x)
  if(is.logical(i))
    i <- seq(i)[i]
  if(is.numeric(i)){
    if(is.ti(i)) i <- i + 1 - start(x)
    else if(couldBeTi(i, tif = tif))
      i <- as.ti(i) + 1 - start(x)
    i[i<=0] <- NA
  }
  else stop("non-numeric row index")

  if(is.matrix(x)){
    if(missing(j))
      return(unclass(x)[i, , drop=drop])
    else
      return(unclass(x)[i, j, drop=drop])
  }
  else
    return(unclass(x)[i])
}

"[<-.tis" <- function(x, i, j, ..., value){
  tif <- tif(x)
  xStart <- start(x)
  x <- stripTis(x)
  if(missing(i)){
    if(missing(j)) x[]   <- value
    else           x[,j] <- value
  }
  else {
    ii <- i
    ## if(is.logical(i)) i <- seq(i)[i]
    if(is.numeric(i)){
      if(!is.ti(i) && couldBeTi(i, tif = tif))
        i <- as.ti(i)
      if(is.ti(i)){
        i <- i + 1 - xStart
        if(any(i < 1)){
          newRows <- 1 - min(i)
          xStart <- xStart - newRows
          if(is.null(m <- ncol(x)))  m <- 1
          i <- i + newRows
          if(is.matrix(x))
            x <- rbind(matrix(NA, newRows, m), x)
          else
            x <- c(rep(NA, newRows), x)
        }
      }
    }
    else if(!is.logical(i)) stop("non-numeric, non-logical row index")
   if(is.matrix(x)){
      if(any(i > nrow(x))){
        newRows <- max(i) - nrow(x)
        x <- rbind(x, matrix(NA, newRows, ncol(x)))
      }
      if(missing(j)){
        if(is.matrix(i))  x[i] <- value
        else {
          if(is.logical(i))
            x[i,] <- rep(value, length = sum(i)*ncol(x))
          else
            x[i,] <- rep(value, length = length(i)*ncol(x))
        }
      }
      else x[i,j] <- value
    }
    else x[i] <- value
  }
  start(x) <- xStart
  class(x) <- c("tis", class(x))
  x
}

head.tis <- function(x, n = 6, ...){
  z <- head(x, n = n, ...)
  start(z) <- start(x)
  z
}

tail.tis <- function(x, n = 6, ...){
  z <- tail(x, n = n, ...)
  start(z) <- end(x) - NROW(z)
  z
}
