nextBusinessDay <- function(x, holidays = NULL, goodFriday = F, board = F){
  ## returns a business-day ti
  bDay <- ti(x, "business")
  weekend <- jul(x) != jul(bDay)
  z <- bDay - weekend + 1
  if(missing(holidays))
    holidays <- holidays(year(z), goodFriday = goodFriday, board = board)
  hol <- match(ymd(z), holidays, nomatch = 0) > 0
  z[hol] <- z[hol] + 1
  z
}

previousBusinessDay <- function(x, holidays = NULL, goodFriday = F, board = F){
  ## returns a business-day ti
  z <- ti(x, "business") - 1
  if(missing(holidays))
    holidays <- holidays(year(z), goodFriday = goodFriday, board = board)
  hol <- match(ymd(z), holidays, nomatch = 0) > 0
  z[hol] <- z[hol] - 1
  z
}

isHoliday <- function(x, goodFriday = F, board = F){
  hols <- holidays(year(x), goodFriday = goodFriday, board = board)
  match(ymd(x), hols, nomatch = 0) > 0
}

isGoodFriday <- function(x){
  hols <- goodFriday(year(x))
  match(ymd(x), hols, nomatch = 0) > 0
}

isEaster <- function(x){
  hols <- easter(year(x))
  match(ymd(x), hols, nomatch = 0) > 0
}

holidays <- function(years, goodFriday = F, board = F){
  hols <- federalHolidays(years, board = board)
  if(goodFriday) hols <- sort(c(hols, goodFriday(years)))
  hols
}

federalHolidays <- function(years, board = F){
  ## returns yyyymmdd dates of federal holidays for given years.
  ##
  ## Federal law defines 10 holidays.  4 set by date (NewYears,
  ## Independence, Veterans, and Christmas) and 6 set by day of the week and
  ## month (MLK, Presidents, Memorial, Labor, Columbus, and Thanksgiving).
  ##
  ## If one of the four fixed-date holidays falls on a Sunday, the federal
  ## holiday is celebrated the next day (Monday).  If it falls on a Saturday,
  ## the preceding day (Friday) is a holiday for the Federal Reserve Board,
  ## but not for the Reserve Banks and the banking system as a whole.
 
  ye4 <- years*10000
  holNames <- c("NewYears", "MLKing", "Presidents",
                "Memorial", "Independence", "Labor",
                "Columbus", "Veterans", "Thanksgiving",
                "Christmas")
  z <- c(ye4 +  101,
         ymd(ti(ye4 +  115, "wmonday")),
         ymd(ti(ye4 +  215, "wmonday")),
         ymd(ti(ye4 +  601, "wmonday") - 1),
         ye4 +  704,
         ymd(ti(ye4 +  901, "wmonday")),
         ymd(ti(ye4 + 1008, "wmonday")),
         ye4 + 1111,
         ymd(ti(ye4 + 1122, "wthursday")),
         ye4 + 1225)
  hols <- sort(z)
  names(hols) <- rep(holNames, length(years))
  if(any(years < 1986)){  ## no MLK before 1986
    drop <- year(hols) < 1986 & names(hols) == "MLKing"
    hols <- hols[!drop]
  }
  weekday <- dayOfWeek(hols)
  hols[weekday == 1] <- ymd(jul(hols[weekday == 1]) + 1)
  if(board)
    hols[weekday == 7] <- ymd(jul(hols[weekday == 7]) - 1)
  hols
}

goodFriday <- function(years){
  ## yyyymmdd dates of Good Friday for given years
  ymd(jul(easter(years)) - 2)
}

easter <- function(years){
  ## yyyymmdd dates of Easter for supplied 4 digit years
  G <- years %% 19
  C <- years %/% 100
  H <- (C - (C %/% 4) - ((8*C + 13) %/% 25) + 19*G + 15) %% 30
  I <- H - (H %/% 28) * (1 - (H %/% 28)*(29 %/% (H + 1))*((21 - G) %/% 11))
  J <- (years + (years %/% 4) + I + 2 - C + (C %/% 4)) %% 7
  L <-  I - J
  month <- 3 + (L + 40) %/% 44
  day <- L + 28 - 31*(month %/% 4)
  10000*years + 100*month + day
}

holidaysBetween <- function(startTi, endTi, goodFriday = F, board = F){
  startTi <- tiDaily(startTi)
  endTi   <- tiDaily(endTi)
  years <- year(startTi):year(endTi)
  hols <- holidays(years, goodFriday, board)
  hols[between(hols, ymd(startTi), ymd(endTi))]
}
