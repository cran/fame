badClassStop <- function(x, class){
  if(!is(x, class))
    stop(paste(deparse(substitute(x)), "is not a", class))
}
