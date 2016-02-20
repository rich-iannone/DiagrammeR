#' Construct the date axis
#' @description Create the date axis when provided
#' points that correspond to dates.
#' @param series_pts an NDF containing points where
#' the \code{x_format} attribute is of type \code{date}.
#' @param plot_data_within_month a choice for whether
#' to display an entire month of days where the data
#' corresponds to points contained within a single
#' month.
#' @export date_axis

date_axis <- function(series_pts,
                      plot_data_within_month = FALSE){

  # Get the day numbers
  day_numbers <-
    as.numeric(format(as.POSIXct(
      series_pts$x,
      origin = "1970-01-01",
      tz = "GMT"), "%d"))

  # Get the month numbers
  month_numbers <-
    as.numeric(format(as.POSIXct(
      series_pts$x,
      origin = "1970-01-01",
      tz = "GMT"), "%m"))

  # Get the year numbers
  year_numbers <-
    as.numeric(format(as.POSIXct(
      series_pts$x,
      origin = "1970-01-01",
      tz = "GMT"), "%Y"))

  # Get range of years for data
  years_in_data <-
    as.numeric(format(as.POSIXct(
      min(series_pts$x),
      origin = "1970-01-01",
      tz = "GMT"), "%Y")):
    as.numeric(format(as.POSIXct(
      max(series_pts$x),
      origin = "1970-01-01",
      tz = "GMT"), "%Y"))

  # Get the number of days in range of data
  total_days_in_data <-
    days_in_data_range(series_pts$x)

  # Get series of days as integer dates
  all_days_within_data <-
    seq(from = min(series_pts$x),
        to = max(series_pts$x),
        by = 86400)

  # Get range of days for data
  days_in_data <-
    as.numeric(format(as.POSIXct(
      all_days_within_data,
      origin = "1970-01-01",
      tz = "GMT"), "%d"))

  # Check condition that all data lies within
  # the same month
  all_data_within_a_month <-
    ifelse(all(month_numbers == month_numbers[1]) &
             all(year_numbers == year_numbers[1]),
           TRUE, FALSE)

  if (all_data_within_a_month == TRUE &
      plot_data_within_month == TRUE){

    # Get the month number
    the_month_number <-
      as.numeric(format(max(as.POSIXct(
        series_pts$x,
        origin = "1970-01-01",
        tz = "GMT")), "%m"))

    # Get the number of days in the month
    days_in_the_month <- days_in_month(series_pts$x[1])

    # Get the abbreviated name of the month
    the_month_abbrev_name <-
      format(max(as.POSIXct(
        series_pts$x,
        origin = "1970-01-01",
        tz = "GMT")), "%b")

    # Get a fractional scale for all tick marks across the span
    tick_fractions <-
      seq(0, (days_in_the_month - 1))/(days_in_the_month - 1)

    major_tick_fractional <-
      tick_fractions[seq(1, length(tick_fractions), 2)]

    minor_tick_fractional <-
      tick_fractions[seq(2, length(tick_fractions), 2)]

    tick_label_locations_fractional <-
      c(-0.05,
        tick_fractions[seq(1, length(tick_fractions), 2)])

    if (days_in_the_month %% 2 == 0){

      tick_label_just <-
        c("\\r", rep("", length(tick_label_locations_fractional) - 1))

      tick_label_values <-
        paste0(c(the_month_abbrev_name,
                 as.character(seq(1, length(tick_fractions), 2))),
               tick_label_just)
    }

    if (days_in_the_month %% 2 != 0){

      tick_label_just <-
        c("\\r", rep("", length(tick_label_locations_fractional) - 1))

      tick_label_values <-
        paste0(c(the_month_abbrev_name,
                 as.character(seq(1, length(tick_fractions), 2))),
               tick_label_just)
    }

    axis_defs <-
      list(tick_fractions = tick_fractions,
           major_tick_fractional = major_tick_fractional,
           minor_tick_fractional = minor_tick_fractional,
           tick_label_locations_fractional = tick_label_locations_fractional,
           tick_label_values = tick_label_values)

    return(axis_defs)
  }

  # Get integer date values for all months in years of dataset
  for (i in 1:length(years_in_data)){
    if (i == 1){
      month_int_dates <- vector(mode = "numeric")
    }
    for (j in 1:12){
      month_int_dates <-
        c(month_int_dates,
          ISOdatetime(
            year = years_in_data[i],
            month = j,
            day = 1, hour = 0,
            min = 0, sec = 0, tz = "GMT"))
    }
  }

  # Get integer date values for the 15th day in each month
  for (i in 1:length(years_in_data)){
    if (i == 1){
      month_15_day_int_dates <- vector(mode = "numeric")
    }
    for (j in 1:12){
      month_15_day_int_dates <-
        c(month_15_day_int_dates,
          ISOdatetime(
            year = years_in_data[i],
            month = j,
            day = 15, hour = 0,
            min = 0, sec = 0, tz = "GMT"))
    }
  }

  # Get integer date values for odd months: 1, 3, 5, 7, 9, 11
  for (i in 1:length(years_in_data)){
    if (i == 1){
      odd_month_int_dates <- vector(mode = "numeric")
    }
    for (j in c(1, 3, 5, 7, 9, 11)){
      odd_month_int_dates <-
        c(odd_month_int_dates,
          ISOdatetime(
            year = years_in_data[i],
            month = j,
            day = 15, hour = 0,
            min = 0, sec = 0, tz = "GMT"))
    }
  }

  # Get integer date values for all years in dataset
  for (i in 1:length(years_in_data)){
    if (i == 1){
      year_int_dates <- vector(mode = "numeric")
    }
    year_int_dates <-
      c(year_int_dates,
        ISOdatetime(
          year = years_in_data[i],
          month = 1,
          day = 1, hour = 0,
          min = 0, sec = 0, tz = "GMT"))
  }

  month_int_dates_within_data <-
    sort(intersect(
      month_int_dates[which(
        month_int_dates >= min(series_pts$x))],
      month_int_dates[which(
        month_int_dates <= max(series_pts$x))]))

  month_15_day_int_dates_within_data <-
    sort(intersect(
      month_15_day_int_dates[which(
        month_15_day_int_dates >= min(series_pts$x))],
      month_15_day_int_dates[which(
        month_15_day_int_dates <= max(series_pts$x))]))

  odd_month_int_dates_within_data <-
    sort(intersect(
      odd_month_int_dates[which(
        odd_month_int_dates >= min(series_pts$x))],
      odd_month_int_dates[which(
        odd_month_int_dates <= max(series_pts$x))]))

  year_int_dates_within_data <-
    sort(intersect(
      year_int_dates[which(
        year_int_dates >= min(series_pts$x))],
      year_int_dates[which(
        year_int_dates <= max(series_pts$x))]))

  month_abbrev_within_data <-
    format(as.POSIXct(
      month_int_dates_within_data,
      origin = "1970-01-01",
      tz = "GMT"), "%b")

  year_number_within_data <-
    format(as.POSIXct(
      year_int_dates_within_data,
      origin = "1970-01-01",
      tz = "GMT"), "%Y")

  # Get a fractional scale for all tick marks across the span
  tick_fractions <-
    seq(0, (total_days_in_data - 1))/(total_days_in_data - 1)

  if (total_days_in_data < 15){

    major_tick_fractional <-
      tick_fractions

    minor_tick_fractional <-
      NULL

    tick_label_locations_fractional <-
      tick_fractions

    tick_label_just <-
      rep("", length(tick_label_locations_fractional))

    tick_label_values <-
      days_in_data
  }

  if (total_days_in_data >= 15 & total_days_in_data < 60){

    major_tick_fractional <-
      tick_fractions[which(all_days_within_data %in% month_int_dates_within_data)]

    minor_tick_fractional <-
      all_days_within_data

    tick_label_locations_fractional <-
      tick_fractions[which(all_days_within_data %in% month_int_dates_within_data)]

    tick_label_just <-
      rep("", length(major_tick_fractional))

    tick_label_values <-
      c(paste0(month_abbrev_within_data[1], "\n",
               year_number_within_data[1],
               tick_label_just[1]),
        paste0(month_abbrev_within_data[-1], "\n",
               ifelse(month_abbrev_within_data[-1] == "Jan",
                      year_number_within_data[-1],
                      " "),
               tick_label_just[-1]))

  }

  if (total_days_in_data >= 60 & total_days_in_data < 1140){

    major_tick_fractional <-
      tick_fractions[which(all_days_within_data %in% month_int_dates_within_data)]

    minor_tick_fractional <-
      tick_fractions[which(all_days_within_data %in% month_15_day_int_dates)]

    tick_label_locations_fractional <-
      major_tick_fractional

    tick_label_just <-
      rep("", length(tick_label_locations_fractional))

    tick_label_values <-
      c(paste0(month_abbrev_within_data[1], "\n",
               year_number_within_data[1],
               tick_label_just[1]),
        paste0(month_abbrev_within_data[-1], "\n",
               ifelse(month_abbrev_within_data[-1] == "Jan",
                      year_number_within_data[-1],
                      " "),
               tick_label_just[-1]))

    if (total_days_in_data > 750){
      tick_label_values <-
        gsub("^([A-Z])([a-z][a-z])(.*)", "\\1\\3",
             tick_label_values)
    }
  }

  if (total_days_in_data >= 1140 & total_days_in_data < 1300){

    major_tick_fractional <-
      tick_fractions[which(all_days_within_data %in% month_int_dates_within_data)]

    minor_tick_fractional <- NULL

    tick_label_locations_fractional <-
      major_tick_fractional

    tick_label_just <-
      rep("", length(tick_label_locations_fractional))

    tick_label_values <-
      c(paste0(month_abbrev_within_data[1], "\n",
               year_number_within_data[1],
               tick_label_just[1]),
        paste0(month_abbrev_within_data[-1], "\n",
               ifelse(month_abbrev_within_data[-1] == "Jan",
                      year_number_within_data[-1],
                      " "),
               tick_label_just[-1]))

    if (total_days_in_data > 750){
      tick_label_values <-
        gsub("^([A-Z])([a-z][a-z])(.*)", "\\1\\3",
             tick_label_values)
    }
  }

  if (total_days_in_data >= 1300){

    major_tick_fractional <-
      tick_fractions[which(all_days_within_data %in% month_int_dates_within_data)]

    minor_tick_fractional <- NULL

    tick_label_locations_fractional <-
      major_tick_fractional

    tick_label_just <-
      rep("", length(tick_label_locations_fractional))

    tick_label_values <-
      c(paste0(month_abbrev_within_data[1], "\n",
               year_number_within_data[1],
               tick_label_just[1]),
        paste0(month_abbrev_within_data[-1], "\n",
               ifelse(month_abbrev_within_data[-1] == "Jan",
                      year_number_within_data[-1],
                      " "),
               tick_label_just[-1]))

    if (total_days_in_data > 750){
      tick_label_values <-
        gsub("^([A-Z])([a-z][a-z])(.*)", "\\1\\3",
             tick_label_values)
    }
  }


  axis_defs <-
    list(tick_fractions = tick_fractions,
         major_tick_fractional = major_tick_fractional,
         minor_tick_fractional = minor_tick_fractional,
         tick_label_locations_fractional = tick_label_locations_fractional,
         tick_label_values = tick_label_values)

  return(axis_defs)
}
