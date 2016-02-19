#' Calculate the number of days in a within a vector
#' of dates
#' @description Calculate the number of days spanning
#' a vector of dates when given those dates as either
#' \code{character} objects or as \code{numeric}
#' objects.
#' @param dates a vector of \code{character} objects
#' in the format \code{YYYY-MM-DD} or \code{numeric}
#' objects where the values are integer numbers of
#' hours.
#' @export days_in_data_range

days_in_data_range <- function(dates){

  if (inherits(dates, "character")){
    date <-
      as.Date(dates,
              origin = "1970-01-01",
              tz = "GMT")
  }

  if (inherits(dates, "integer")){
    dates <-
      as.Date(as.character(
        as.POSIXct(dates,
                   origin = "1970-01-01",
                   tz = "GMT")),
        origin = "1970-01-01",
        tz = "GMT")
  }

  number_of_days <-
    (max(dates) - min(dates))[[1]] + 1

  return(number_of_days)
}
