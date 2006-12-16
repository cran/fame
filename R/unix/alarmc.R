alarmc <- function(seconds = 0){
  .C("alarmc", as.integer(seconds)[1], PACKAGE = "fame")[[1]]
}
