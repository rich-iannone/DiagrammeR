#' Create a series of (x, y) points for a graph
#' @description Create a series of (x, y) points along
#' with other style attributes for plotting on a graph
#' @param series_label a name for the series of (x, y)
#' points.
#' @param x a vector of x points.
#' @param y a vector of y points.
#' @param shape the shape for the points.
#' @param line_width the line width for the shape representing the
#' data point.
#' @param fill_color the fill color for the shape representing the
#' data point. If \code{none} or \code{transparent} provided,
#' then the shape will not be filled with a color.
#' @param line_color the color of the shape line representing the
#' data point.
#' @param width the width of the shape representing the
#' data point.
#' @param height the height of the shape representing the
#' data point.
#' @export create_xy_pts

create_xy_pts <- function(series_label,
                          x,
                          y,
                          size = NULL,
                          shape = "circle",
                          line_width = 2.5,
                          fill_color = "none",
                          line_color = "gray15",
                          width = 0.15,
                          height = 0.15){

  # Stop function if any of several
  # conditions not met
  stopifnot(length(x) == length(y))
  stopifnot(length(x) > 0)
  if (!is.null(size)){
    stopifnot(length(size) == length(x))
  }

  # Combine x and y in a data frame
  xy_df <-
    data.frame(x = x, y = y,
               stringsAsFactors = FALSE)

  # Arrange in ascending order of x
  xy_df <- xy_df[order(xy_df[,1]),]

  # Get sorted vectors of x and y
  x <- xy_df$x
  y <- xy_df$y

  # Determine the format of x
  if (all(grepl("-", x)) & !all(grepl(":", x))){
    x_date <- try(as.Date(x, format = "%Y-%m-%d"))
    if (all(class(x_date) != "try-error") &
        !any(is.na(x_date))) x_format <- "date"
  }

  if (all(grepl("-", x)) & all(grepl(":", x))){
    x_datetime <- try(as.POSIXct(x, format = "%Y-%m-%d %H:%M", tz = "GMT"))
    if (all(class(x_datetime) != "try-error") &
        !any(is.na(x_datetime))) x_format <- "datetime"
  }

  if (inherits(x, "numeric")) x_format <- "numeric"

  # Determine the format of y
  if (all(grepl("-", y)) & !all(grepl(":", y))){
    y_date <- try(as.Date(y, format = "%Y-%m-%d"))
    if (all(class(y_date) != "try-error") &
        !any(is.na(y_date))) y_format <- "date"
  }

  if (all(grepl("-", y)) & all(grepl(":", y))){
    y_datetime <- try(as.POSIXct(y, format = "%Y-%m-%d %H:%M", tz = "GMT"))
    if (all(class(y_datetime) != "try-error") &
        !any(is.na(y_datetime))) y_format <- "datetime"
  }

  if (inherits(y, "numeric")) y_format <- "numeric"

  # Perform conversions of date/date-time to numeric values
  if (x_format %in% c("datetime", "date")){
    x <- as.integer(as.POSIXct(x, tz = "GMT"))
  }

  if (y_format %in% c("datetime", "date")){
    y <- as.integer(as.POSIXct(x, tz = "GMT"))
  }

  # Count the number of points in the plot
  point_count <- length(x)

  # Create the NDF that contains a set
  # of (x, y) points alongside attributes
  chart_nodes <-
    create_nodes(
      nodes = paste0(series_label, "_", 1:point_count),
      type = series_label,
      label = " ",
      graph_component = "xy_pts",
      x = x,
      y = y,
      x_format = x_format,
      y_format = y_format,
      shape = ifelse(fill_color %in%
                       c("none", "transparent") &
                       line_color == "none",
                     "point", shape),
      penwidth = line_width,
      fillcolor = ifelse(fill_color %in%
                           c("none", "transparent"),
                         "#FFFFFF00", fill_color),
      color = ifelse(line_color == "none", "#FFFFFF00",
                     line_color),
      fixedsize = "true",
      width = ifelse(fill_color %in%
                       c("none", "transparent") &
                       line_color == "none",
                     0, width),
      height = ifelse(fill_color %in%
                       c("none", "transparent") &
                       line_color == "none",
                     0, height))

  return(chart_nodes)
}
