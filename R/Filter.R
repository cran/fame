Filter <- function(x, ...) UseMethod("Filter")
Filter.tis <- function(x, ...)     naWindow(tis(filter(x, ...), start = start(x)))
Filter.default <- function(x, ...) naWindow(filter(x, ...))

