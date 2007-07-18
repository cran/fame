tzOffset <- function(tz = "", tm = Sys.time()){
  ## offset of time zone tz from UTC (aka GMT) in seconds.
  ## tz = "" denotes local time zone
  as.vector(unclass(as.POSIXct(format(tm, tz = "UTC"))) - unclass(as.POSIXct(format(tm, tz = tz))))
}

