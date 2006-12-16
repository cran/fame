hex2numeric <- function(hex){
  hexDigits <- c(0:9, letters[1:6])
  chars <- strsplit(tolower(hex), split = NULL)
  numbers <- rep(0, length(chars))
  for(i in seq(along = chars))
    for(j in seq(along = chars[[i]]))
      numbers[i] <- 16*numbers[i] + match(chars[[i]][j], hexDigits) - 1
  numbers
}

hexidecimal <- function(dec)  sprintf("%x", dec)
