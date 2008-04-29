is.scalar <- function(x){
  length(x) == 1 && is.numeric(x) && is.finite(x)
}
