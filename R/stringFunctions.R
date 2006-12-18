blanks <- function(n) paste(character(n+1), collapse = " ")

stripBlanks <- function(strings){
  ## strips leading and trailing blanks
  ans <- gsub("^ *(.*)", "\\1", strings)
  notEmpty <- nchar(ans) > 0
  ans[notEmpty] <- gsub(" *$", "", ans[notEmpty])
  ans
}

pad.string <- function(x, len = max(nchar(x)), padchar = " ", right = T){
  ## pads x to desired length
  if(nchar(padchar) != 1) 
    stop("nchar(padchar) != 1")
  template <- paste(rep(padchar, len), collapse = "")
  if(right)
    xpad <- paste(substring(template, 1 + nchar(x)), x, sep = "")
  else  
    xpad <- paste(x, substring(template, 1 + nchar(x)), sep = "")
  attributes(xpad) <- attributes(x)
  xpad
}
