commandLineString <- function(){
  ca <- commandArgs()
  if(is.na(i <- match("--args", ca)))
    character(0)
  else paste(ca[(i+1):length(ca)], collapse = " ")
}
