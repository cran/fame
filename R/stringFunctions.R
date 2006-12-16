blanks <- function(n) paste(character(n+1), collapse = " ")

stripBlanks <- function(strings){
  ## strips leading and trailing blanks
  ans <- gsub("^ *(.*)", "\\1", strings)
  notEmpty <- nchar(ans) > 0
  ans[notEmpty] <- gsub(" *$", "", ans[notEmpty])
  ans
}
