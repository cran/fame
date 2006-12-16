naWindow <- function(x, union = F){
  if(NCOL(x) == 1)
	valid <- !is.na(x)
  else {
	if(union) valid <- !apply(is.na(x), 1, all)
	else      valid <- !apply(is.na(x), 1, any)
  }
  validDates <- ti(x)[valid]
  if(length(validDates) > 0)
    window(x, start = min(validDates), end = max(validDates))
  else x
}

