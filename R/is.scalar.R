isFameScalar <- function(x){
  !(is.tis(x) || is.ts(x)) && is.atomic(x) && length(x) == 1
}

isScalarOrTis <- function(x){
  is.tis(x) || isFameScalar(x)
}

is.scalar <- function(x){
  length(x) == 1 && is.numeric(x) && is.finite(x)
}
