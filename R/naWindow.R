naWindow <- function(x, union = F){
  if(NCOL(x) == 1)
	valid <- !is.na(x)
  else {
	if(union) valid <- !apply(is.na(x), 1, all)
	else      valid <- !apply(is.na(x), 1, any)
  }
  tsx <- is.ts(x)
  if(tsx) x <- as.tis(x)
  validDates <- ti(x)[valid]
  if(length(validDates) > 0)
    z <- window(x, start = min(validDates), end = max(validDates))
  else z <- x
  if(tsx) as.ts(z)
  else z
}

