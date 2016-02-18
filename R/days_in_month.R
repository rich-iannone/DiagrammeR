#' Calculate the number of days in a month
#' @description Calculate the number of days in a
#' month when given either a date as a \code{character}
#' object or as a \code{numeric} object.
#' @param date either a \code{character} object in the
#' format \code{YYYY-MM-DD} or a \code{numeric} object
#' where the value is the integer number of hours.
#' @export days_in_month

days_in_month <- function(date){

  if (inherits(date, "character")){
    date <-
      as.Date(date,
              origin = "1970-01-01",
              tz = "GMT")
  }

  if (inherits(date, "integer")){
    date <-
      as.Date(as.character(
        as.POSIXct(date,
                   origin = "1970-01-01",
                   tz = "GMT")),
        origin = "1970-01-01",
        tz = "GMT")
  }

  m <- format(date, format = "%m")
  while (format(date, format = "%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1,
                           format = "%d")))
}
