description <- function(x)
  attr(x, "description")

"description<-" <- function(x, value){
  attr(x, "description") <- value
  invisible(x)
}

documentation <- function(x)
  attr(x, "documentation")

"documentation<-" <- function(x, value){
  attr(x, "documentation") <- value
  invisible(x)
}
