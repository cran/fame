assignList <- function(aList, pos = -1, envir = as.environment(pos), inherits = FALSE){
  if(is.null(nms <- names(aList))) stop("names(aList) is NULL")
  if(any(nms == "")) stop("blank name")
  for(nm in nms)
    .Internal(assign(nm, aList[[nm]], envir, inherits))
}
