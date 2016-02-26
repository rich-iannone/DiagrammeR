#' Create an (x, y) scatterplot graph
#' @description Create an (x, y) scatterplot graph
#' using an NDF and an EDF for connecting
#' lines.
#' @param ... one or more NDFs containing points
#' (created through the use of the \code{create_xy_pts()}
#' function) or EDFs containing lines (created using
#' the \code{create_xy_lines()} function).
#' @param aspect_ratio the aspect ratio of the plot
#' area.
#' @param x_scale the minimum and maximum x value to
#' display on the x-axis. Not providing values will
#' result in autoscaling in the x direction based on
#' provided data.
#' @param y_scale the minimum and maximum y value to
#' display on the y-axis. Not providing values will
#' result in autoscaling in the y direction based on
#' provided data.
#' @param xy_major_steps the number of major steps
#' across the x and y directions. Not providing values
#' will result in automatically provided major steps.
#' @param x_name an optional x-axis name.
#' @param y_name an optional y-axis name.
#' @param heading text to display as a heading above
#' the plot area. Providing two character values
#' will result in left- and right-justified text on
#' the same line above the plot area. Using
#' consecuctive \code{#} signs increases the header
#' level and has the result of reducing the size of the
#' text.
#' @param footer text to display as a footer below the
#' plot area. Providing two character values
#' will result in left- and right-justified text on
#' the same line below the plot area. Using
#' consecuctive \code{#} signs increases the header
#' level and has the result of reducing the size of the
#' text.
#' @param xy_value_labels the format for displaying
#' values of the x and y axes. Options are
#' \code{numeric} (the default), \code{percentage},
#' \code{date_time} or currency (by use of a 3-letter
#' or 3-number ISO 4217 currency code. If using
#' currency, one can additionally state whether to
#' abbreviate large values with a \code{K} (thousands),
#' an \code{M} (millions), a \code{B} (billions), or a
#' \code{T} (trillions) by appending the currency code
#' with a colon the letter for the abbreviated label
#' (e.g., \code{USD:M}). If a single-length value is
#' provided, then that format type will be applied to
#' both the x and y axes. Providing a vector of length
#' 2 will allow for application of the provided formats
#' to the x and y axes, respectively.
#' @param xy_value_decimals the number of decimal
#' places to show for numeric values labeling the x and
#' y axes. If a single-length value is provided, then
#' that value will be applied to both the x and y
#' axes. Providing a vector of length 2 will apply
#' formatting to the x and y axes, respectively.
#' @param x_name_location the location of the x-axis
#' name, either \code{inside} (default) or \code{outside}.
#' @param x_tick_marks choose whether to show tick
#' marks along the x axis, and where to show them.
#' Options are \code{inside} (default), \code{outside},
#' or \code{centered}.
#' @param y_tick_marks choose whether to show tick
#' marks along the y axis, and where to show them.
#' Options are \code{inside} (default), \code{outside},
#' or \code{centered}.
#' @param include_xy_minima choose whether to show the
#' minimum values for the x and y axes, respectively.
#' @param include_legend an option for whether to
#' include a legend.
#' @param legend_offset the x and y offset (from
#' top-right) of the legend.
#' @param x_value_prefix a fixed prefix to apply to all
#' labels of the x axis.
#' @param y_value_prefix a fixed prefix to apply to all
#' labels of the y axis.
#' @param x_value_suffix a fixed suffix to apply to all
#' labels of the x axis.
#' @param y_value_suffix a fixed suffix to apply to all
#' labels of the y axis.
#' @param bg_color the color for the background of the
#' entire viewable area
#' @examples
#' \dontrun{
#' library(DiagrammeRsvg)
#' library(magrittr)
#'
#' # Create three groups (Setosa, Versicolor, and Virginica)
#' # of x,y datapoints for petal length (x) and petal width (y)
#' setosa <-
#'   create_xy_pts(
#'     series_label = "Setosa",
#'     x = subset(iris, Species == "setosa")$Petal.Length,
#'     y = subset(iris, Species == "setosa")$Petal.Width,
#'     line_color = "red")
#'
#' versicolor <-
#'   create_xy_pts(
#'     series_label = "Versicolor",
#'     x = subset(iris, Species == "versicolor")$Petal.Length,
#'     y = subset(iris, Species == "versicolor")$Petal.Width,
#'     line_color = "green")
#'
#' virginica <-
#'   create_xy_pts(
#'     series_label = "Virginica",
#'     x = subset(iris, Species == "virginica")$Petal.Length,
#'     y = subset(iris, Species == "virginica")$Petal.Width,
#'     line_color = "blue")
#'
#' # Add these xy points to the `create_xy_graph()`
#' # function and add axis titles and a heading
#' iris_length_width <-
#'   create_xy_graph(
#'     setosa,
#'     versicolor,
#'     virginica,
#'     x_name = "Petal Length (cm)",
#'     y_name = "Petal Width (cm)",
#'     heading = "#Iris Dataset",
#'     legend_offset = c(0, 6),
#'     bg_color = "white")
#'
#' # View the graph
#' iris_length_width %>% render_graph
#'
#' # Export the graph to a PDF file
#' iris_length_width %>% export_graph("iris.pdf")
#'
#' # Export the graph to a PDF file
#' iris_length_width %>% export_graph("iris.png")
#' }
#' @import scales
#' @export create_xy_graph

create_xy_graph <- function(...,
                            aspect_ratio = c(1, 1),
                            x_scale = NULL,
                            y_scale = NULL,
                            xy_major_steps = NULL,
                            x_name = NULL,
                            y_name = NULL,
                            heading = NULL,
                            footer = NULL,
                            xy_value_labels = NULL,
                            xy_value_decimals = NULL,
                            x_name_location = "inside",
                            x_tick_marks = "inside",
                            y_tick_marks = "inside",
                            include_xy_minima = c(TRUE, TRUE),
                            include_legend = TRUE,
                            legend_offset = c(0, 0),
                            x_value_prefix = NULL,
                            y_value_prefix = NULL,
                            x_value_suffix = NULL,
                            y_value_suffix = NULL,
                            bg_color = "transparent"){

  # Define basic graph layout properties
  xy_axis_lab_dist <- c(0.0, 0.0)
  xy_axis_tick_width <- c(0.1, 0.1)

  # Take multiple series of points and lines
  pts_lines_df <- list(...)

  if (length(pts_lines_df) > 0){

    for (i in 1:length(pts_lines_df)){
      if (i == 1){
        pts_df <- vector(mode = "numeric")
        lines_df <- vector(mode = "numeric")

        error_list <- vector(mode = "numeric")

        x_date_pts <- vector(mode = "numeric")
        y_date_pts <- vector(mode = "numeric")
      }

      if (all("numeric" %in% pts_lines_df[[i]]$x_format) &
          all("numeric" %in% pts_lines_df[[i]]$y_format)){
        pts_df <- c(pts_df, i)
      }

      if ("from" %in% colnames(pts_lines_df[[i]]) &
          "to" %in% colnames(pts_lines_df[[i]])){
        lines_df <- c(lines_df, i)
      }

      if (inherits(pts_lines_df[[i]], "dgr_graph_xy_err")){
        error_list <- c(error_list, i)
      }

      if (all("date" %in% pts_lines_df[[i]]$x_format)){
        x_date_pts <- c(x_date_pts, i)
      }

      if (all("date" %in% pts_lines_df[[i]]$y_format)){
        y_date_pts <- c(y_date_pts, i)
      }
    }

    if (length(pts_df) == 0){
      series_pts <- NULL
    } else if (length(pts_df) == 1){
      series_pts <- pts_lines_df[[pts_df]]
    } else if (length(pts_df) > 1){
      for (i in 2:length(pts_df)){

        if (i == 2){
          series_pts <-
            combine_nodes(
              pts_lines_df[[pts_df[1]]],
              pts_lines_df[[pts_df[2]]])
        }

        if (i > 2){
          series_pts <-
            combine_nodes(
              series_pts,
              pts_lines_df[[pts_df[i]]])
        }

        if (i == length(pts_df)){
          series_pts$x <- as.numeric(series_pts$x)
          series_pts$y <- as.numeric(series_pts$y)
        }
      }
    }

    if (length(lines_df) == 0){
      series_lines <- NULL
    } else if (length(lines_df) == 1){
      series_lines <- pts_lines_df[[lines_df]]
    } else if (length(lines_df) > 1){
      for (i in 2:length(lines_df)){

        if (i == 2){
          series_lines <-
            combine_edges(
              pts_lines_df[[lines_df[1]]],
              pts_lines_df[[lines_df[2]]])
        }

        if (i > 2){
          series_lines <-
            combine_edges(
              series_lines,
              pts_lines_df[[lines_df[i]]])
        }
      }
    }

    if (length(error_list) == 1){

      pts_lines_df[[error_list]][[1]]$x <-
        as.numeric(pts_lines_df[[error_list]][[1]]$x)

      pts_lines_df[[error_list]][[1]]$y <-
        as.numeric(pts_lines_df[[error_list]][[1]]$y)

      series_pts <-
        combine_nodes(
          series_pts,
          pts_lines_df[[error_list]][[1]]
        )

      series_lines <-
        combine_edges(
          series_lines,
          pts_lines_df[[error_list]][[2]]
        )
    }

    if (length(x_date_pts) == 0){
      series_x_date_pts <- NULL
    } else if (length(x_date_pts) == 1){
      series_x_date_pts <- pts_lines_df[[x_date_pts]]
    } else if (length(x_date_pts) > 1){
      for (i in 2:length(x_date_pts)){

        if (i == 2){
          series_x_date_pts <-
            combine_edges(
              pts_lines_df[[x_date_pts[1]]],
              pts_lines_df[[x_date_pts[2]]])
        }

        if (i > 2){
          series_x_date_pts <-
            combine_edges(
              series_x_date_pts,
              pts_lines_df[[x_date_pts[i]]])
        }
      }
    }

    if (length(y_date_pts) == 0){
      series_y_date_pts <- NULL
    } else if (length(y_date_pts) == 1){
      series_y_date_pts <- pts_lines_df[[y_date_pts]]
    } else if (length(y_date_pts) > 1){
      for (i in 2:length(y_date_pts)){

        if (i == 2){
          series_y_date_pts <-
            combine_edges(
              pts_lines_df[[y_date_pts[1]]],
              pts_lines_df[[y_date_pts[2]]])
        }

        if (i > 2){
          series_y_date_pts <-
            combine_edges(
              series_y_date_pts,
              pts_lines_df[[y_date_pts[i]]])
        }
      }
    }

    # If `x_scale`, `y_scale`, and `xy_major_steps` not
    # provided, devise bounds and breaks
    if (is.null(x_scale) &
        is.null(y_scale) &
        is.null(xy_major_steps) &
        (!is.null(series_pts)) |
        !is.null(series_x_date_pts) |
        !is.null(series_y_date_pts)){

      if (!is.null(series_pts)){
        x_scale <-
          c(cbreaks(c(min(as.numeric(series_pts$x)),
                      max(as.numeric(series_pts$x))),
                    pretty_breaks(10))[[1]][1],
            tail(cbreaks(c(min(as.numeric(series_pts$x)),
                           max(as.numeric(series_pts$x))),
                         pretty_breaks(10))[[1]], 1))

        y_scale <-
          c(cbreaks(c(min(as.numeric(series_pts$y)),
                      max(as.numeric(series_pts$y))),
                    pretty_breaks(10))[[1]][1],
            tail(cbreaks(c(min(as.numeric(series_pts$y)),
                           max(as.numeric(series_pts$y))),
                         pretty_breaks(10))[[1]], 1))

        xy_major_steps <-
          c(length(cbreaks(c(min(as.numeric(series_pts$x)),
                             max(as.numeric(series_pts$x))),
                           pretty_breaks(10))[[1]]) - 1,
            length(cbreaks(c(min(as.numeric(series_pts$y)),
                             max(as.numeric(series_pts$y))),
                           pretty_breaks(10))[[1]]) - 1)
      }

      if (!is.null(series_x_date_pts)){

        y_scale <-
          c(cbreaks(c(min(as.numeric(series_x_date_pts$y)),
                      max(as.numeric(series_x_date_pts$y))),
                    pretty_breaks(10))[[1]][1],
            tail(cbreaks(c(min(as.numeric(series_x_date_pts$y)),
                           max(as.numeric(series_x_date_pts$y))),
                         pretty_breaks(10))[[1]], 1))

        xy_major_steps <-
          c(length(cbreaks(c(min(as.numeric(series_x_date_pts$x)),
                             max(as.numeric(series_x_date_pts$x))),
                           pretty_breaks(10))[[1]]) - 1,
            length(cbreaks(c(min(as.numeric(series_x_date_pts$y)),
                             max(as.numeric(series_x_date_pts$y))),
                           pretty_breaks(10))[[1]]) - 1)
      }

      if (!is.null(series_y_date_pts)){
        x_scale <-
          c(cbreaks(c(min(as.numeric(series_y_date_pts$x)),
                      max(as.numeric(series_y_date_pts$x))),
                    pretty_breaks(10))[[1]][1],
            tail(cbreaks(c(min(as.numeric(series_y_date_pts$x)),
                           max(as.numeric(series_y_date_pts$x))),
                         pretty_breaks(10))[[1]], 1))

        y_scale <-
          c(cbreaks(c(min(as.numeric(series_y_date_pts$y)),
                      max(as.numeric(series_y_date_pts$y))),
                    pretty_breaks(10))[[1]][1],
            tail(cbreaks(c(min(as.numeric(series_y_date_pts$y)),
                           max(as.numeric(series_y_date_pts$y))),
                         pretty_breaks(10))[[1]], 1))

        xy_major_steps <-
          c(length(cbreaks(c(min(as.numeric(series_y_date_pts$x)),
                             max(as.numeric(series_y_date_pts$x))),
                           pretty_breaks(10))[[1]]) - 1,
            length(cbreaks(c(min(as.numeric(series_y_date_pts$y)),
                             max(as.numeric(series_y_date_pts$y))),
                           pretty_breaks(10))[[1]]) - 1)
      }
    }
  }

  if (length(pts_lines_df) == 0){
    if (is.null(x_scale) &
        is.null(y_scale) &
        is.null(xy_major_steps)){

      x_scale <- c(0, 10)
      y_scale <- c(0, 10)
      xy_major_steps <- c(10, 10)
      series_pts <- NULL
      series_lines <- NULL
    }
  }

  # Define the x-span and the y-span
  # by the aspect ratio
  x_span <- 10 * aspect_ratio[1]
  y_span <- 10 * aspect_ratio[2]

  # Rescale series data and subset by chart bounds
  if (!is.null(series_pts)){
    series_pts$x <- rescale(as.numeric(series_pts$x),
                            to = c(0, x_span),
                            from = c(x_scale[1], x_scale[2]))
    series_pts$y <- rescale(as.numeric(series_pts$y),
                            to = c(0, y_span),
                            from = c(y_scale[1], y_scale[2]))
    series_pts <- subset(series_pts, x >= 0)
    series_pts <- subset(series_pts, y >= 0)
    series_pts <- subset(series_pts, x <= x_span)
    series_pts <- subset(series_pts, y <= y_span)
  }

  if (!is.null(series_x_date_pts)){

    axis_defs <- date_axis(series_pts = series_x_date_pts)

    series_x_date_pts$x <- rescale(as.numeric(series_x_date_pts$x),
                                   to = c(0, x_span))
    series_x_date_pts$x <- round(series_x_date_pts$x, digits = 3)
    series_x_date_pts$y <- rescale(as.numeric(series_x_date_pts$y),
                                   to = c(0, y_span),
                                   from = c(y_scale[1], y_scale[2]))
    series_x_date_pts <- subset(series_x_date_pts, x >= 0)
    series_x_date_pts <- subset(series_x_date_pts, y >= 0)
    series_x_date_pts <- subset(series_x_date_pts, x <= x_span)
    series_x_date_pts <- subset(series_x_date_pts, y <= y_span)
  }

  if (!is.null(xy_value_decimals)){
    x_value_decimals <- xy_value_decimals[1]
  } else {
    x_value_decimals <- 1
  }

  if (!is.null(xy_value_decimals) & length(xy_value_decimals) == 2){
    y_value_decimals <- xy_value_decimals[2]
  } else if (!is.null(xy_value_decimals) & length(xy_value_decimals) == 1){
    y_value_decimals <- xy_value_decimals[1]
  } else {
    y_value_decimals <- 1
  }

  if (!is.null(xy_value_labels)){
    x_value_labels <- xy_value_labels[1]
  } else {
    x_value_labels <- "numeric"
  }

  if (!is.null(xy_value_labels) & length(xy_value_labels) == 2){
    y_value_labels <- xy_value_labels[2]
  } else if (!is.null(xy_value_labels) & length(xy_value_labels) == 1){
    y_value_labels <- xy_value_labels[1]
  } else {
    y_value_labels <- "numeric"
  }

  # Format the `x_labels` object
  if (x_value_labels == "numeric"){
    x_labels <-
      formatC(
        seq(x_scale[1], x_scale[2],
            ((x_scale[2] - x_scale[1]) / xy_major_steps[1])),
        digits = x_value_decimals,
        format = "f")
  } else if (x_value_labels == "percentage"){
    x_labels <-
      paste0(
        formatC(
          seq(x_scale[1], x_scale[2],
              ((x_scale[2] - x_scale[1]) / xy_major_steps[1])) * 100,
          digits = x_value_decimals,
          format = "f"), "%")
  } else if (x_value_labels == "date"){
    x_labels <- axis_defs$tick_label_values
  } else if (gsub("([A-Z][A-Z][A-Z]).*", "\\1", x_value_labels) %in%
             currency()$iso_4217_code){

    if (grepl("[A-Z][A-Z][A-Z]:(K|k)", x_value_labels)){
      divisor <- 1000
      suffix <- "K"
    } else if (grepl("[A-Z][A-Z][A-Z]:(M|m)", x_value_labels)){
      divisor <- 1000000
      suffix <- "M"
    } else if (grepl("[A-Z][A-Z][A-Z]:(B|b)", x_value_labels)){
      divisor <- 1000000000
      suffix <- "B"
    } else if (grepl("[A-Z][A-Z][A-Z]:(T|t)", x_value_labels)){
      divisor <- 1000000000000
      suffix <- "T"
    } else {
      divisor <- 1
      suffix <- ""
    }

    currency_code <- gsub("([A-Z][A-Z][A-Z]).*", "\\1", x_value_labels)

    values <-
      seq(x_scale[1], x_scale[2],
          ((x_scale[2] - x_scale[1]) /
             xy_major_steps[1])) / divisor

    digits <-
      ifelse(x_scale[2]/divisor <= 5,
             currency()[which(currency()$iso_4217_code %in%
                                currency_code), 3], 0)

    x_labels <-
      paste0(
        currency()[which(currency()$iso_4217_code %in%
                           currency_code),5],
        formatC(values,
                digits = ifelse(x_scale[2]/divisor <= 5,
                                digits, 0), format = "f"), suffix)


  } else if (gsub("([0-9][0-9][0-9]).*", "\\1", x_value_labels) %in%
             currency()$curr_number){

    if (grepl("[0-9][0-9][0-9]:(K|k)", x_value_labels)){
      divisor <- 1000
      suffix <- "K"
    } else if (grepl("[0-9][0-9][0-9]:(M|m)", x_value_labels)){
      divisor <- 1000000
      suffix <- "M"
    } else if (grepl("[0-9][0-9][0-9]:(B|b)", x_value_labels)){
      divisor <- 1000000000
      suffix <- "B"
    } else if (grepl("[0-9][0-9][0-9]:(T|t)", x_value_labels)){
      divisor <- 1000000000000
      suffix <- "T"
    } else {
      divisor <- 1
      suffix <- ""
    }

    currency_code <- gsub("([0-9][0-9][0-9]).*", "\\1", x_value_labels)

    values <-
      seq(x_scale[1], x_scale[2],
          ((x_scale[2] - x_scale[1]) /
             xy_major_steps[1])) / divisor

    digits <-
      ifelse(x_scale[2]/divisor <= 5,
             currency()[which(currency()$curr_number %in%
                                currency_code), 3], 0)

    x_labels <-
      paste0(
        currency()[which(currency()$curr_number %in%
                           currency_code),5],
        formatC(values,
                digits = ifelse(x_scale[2]/divisor <= 5,
                                digits, 0), format = "f"), suffix)
  }

  # Format the `y_labels` object
  if (y_value_labels == "numeric"){
    y_labels <-
      formatC(
        seq(y_scale[1], y_scale[2],
            ((y_scale[2] - y_scale[1]) / xy_major_steps[2])),
        digits = y_value_decimals,
        format = "f")
  } else if (y_value_labels == "percentage"){
    y_labels <-
      paste0(formatC(
        seq(y_scale[1], y_scale[2],
            ((y_scale[2] - y_scale[1]) / xy_major_steps[2])) * 100,
        digits = y_value_decimals,
        format = "f"), "%")
  } else if (y_value_labels == "date"){
    y <- as.Date(as.POSIXct(y_scale, origin = "1970-01-01", tz = "GMT"))
    t <- date_trans()
    y_labels <- t$format(t$breaks(range(y)))
    xy_major_steps[2] <- length(t$format(t$breaks(range(y)))) - 1
  } else if (gsub("([A-Z][A-Z][A-Z]).*", "\\1", y_value_labels) %in%
             currency()$iso_4217_code){

    if (grepl("[A-Z][A-Z][A-Z]:(K|k)", y_value_labels)){
      divisor <- 1000
      suffix <- "K"
    } else if (grepl("[A-Z][A-Z][A-Z]:(M|m)", y_value_labels)){
      divisor <- 1000000
      suffix <- "M"
    } else if (grepl("[A-Z][A-Z][A-Z]:(B|b)", y_value_labels)){
      divisor <- 1000000000
      suffix <- "B"
    } else if (grepl("[A-Z][A-Z][A-Z]:(T|t)", y_value_labels)){
      divisor <- 1000000000000
      suffix <- "T"
    } else {
      divisor <- 1
      suffix <- ""
    }

    currency_code <- gsub("([A-Z][A-Z][A-Z]).*", "\\1", y_value_labels)

    values <-
      seq(y_scale[1], y_scale[2],
          ((y_scale[2] - y_scale[1]) /
             xy_major_steps[2])) / divisor

    digits <-
      ifelse(y_scale[2]/divisor <= 5,
             currency()[which(currency()$iso_4217_code %in%
                                currency_code), 3], 0)

    y_labels <-
      paste0(
        currency()[which(currency()$iso_4217_code %in%
                           currency_code),5],
        formatC(values,
                digits = ifelse(y_scale[2]/divisor <= 5,
                                digits, 0), format = "f"), suffix)

  } else if (gsub("([0-9][0-9][0-9]).*", "\\1", y_value_labels) %in%
             currency()$curr_number){

    if (grepl("[0-9][0-9][0-9]:(K|k)", y_value_labels)){
      divisor <- 1000
      suffix <- "K"
    } else if (grepl("[0-9][0-9][0-9]:(M|m)", y_value_labels)){
      divisor <- 1000000
      suffix <- "M"
    } else if (grepl("[0-9][0-9][0-9]:(B|b)", y_value_labels)){
      divisor <- 1000000000
      suffix <- "B"
    } else if (grepl("[0-9][0-9][0-9]:(T|t)", y_value_labels)){
      divisor <- 1000000000000
      suffix <- "T"
    } else {
      divisor <- 1
      suffix <- ""
    }

    currency_code <- gsub("([0-9][0-9][0-9]).*", "\\1", y_value_labels)

    values <-
      seq(y_scale[1], y_scale[2],
          ((y_scale[2] - y_scale[1]) /
             xy_major_steps[2])) / divisor

    digits <-
      ifelse(y_scale[2]/divisor <= 5,
             currency()[which(currency()$curr_number %in%
                                currency_code), 3], 0)

    y_labels <-
      paste0(
        currency()[which(currency()$curr_number %in%
                           currency_code),5],
        formatC(values,
                digits = ifelse(y_scale[2]/divisor <= 5,
                                digits, 0), format = "f"), suffix)
  }

  # Add prefixes if supplied
  if (!is.null(x_value_prefix)){
    x_labels <- paste0(x_value_prefix, x_labels)
  }

  if (!is.null(y_value_prefix)){
    y_labels <- paste0(y_value_prefix, y_labels)
  }

  # Add suffixes if supplied
  if (!is.null(x_value_suffix)){
    x_labels <- paste0(x_labels, x_value_suffix)
  }

  if (!is.null(y_value_suffix)){
    y_labels <- paste0(y_labels, y_value_suffix)
  }

  # Remove extraneous series lines
  if (!is.null(series_lines)){
    if (!exists("series_pts")){
    series_lines <-
      series_lines[which(series_lines$from %in% series_pts$nodes),]
    series_lines <-
      series_lines[which(series_lines$to %in% series_pts$nodes),]
    }
  }

  # Define the graph heading
  if (!is.null(heading)){

    heading_levels <- nchar(gsub("^(#*).*", "\\1", heading))

    if (any(heading_levels > 0)){
      heading_fontsize <- vector(mode = "numeric")
      for (i in 1:length(heading_levels)){
        if (heading_levels[i] == 0) heading_fontsize[i] <- 24
        if (heading_levels[i] == 1) heading_fontsize[i] <- 32
        if (heading_levels[i] == 2) heading_fontsize[i] <- 24
        if (heading_levels[i] == 3) heading_fontsize[i] <- 19
        if (heading_levels[i] == 4) heading_fontsize[i] <- 16
        if (heading_levels[i] == 5) heading_fontsize[i] <- 13
        if (heading_levels[i] == 6) heading_fontsize[i] <- 11
      }

      heading <- gsub("^(#| )*(.*)", "\\2", heading)
    }

    heading_node <-
      create_nodes(
        nodes = "heading",
        type = "graph_component",
        label = paste0(heading[1], "\\l"),
        graph_component = "heading_left",
        labelloc = "b",
        x = x_span/2,
        y = 10.5,
        fontname = "Helvetica",
        fontsize = heading_fontsize[1],
        fontcolor = "gray20",
        width = x_span * 1.15,
        height = 0.5,
        shape = "plaintext")

    if (length(heading) > 1){
      heading_right_node <-
        create_nodes(
          nodes = "heading_right",
          type = "graph_component",
          label = paste0(heading[2], "\\r"),
          graph_component = "heading_right",
          labelloc = "b",
          x = x_span/2,
          y = 10.5,
          fontname = "Helvetica",
          fontsize = heading_fontsize[2],
          fontcolor = "gray20",
          width = x_span,
          height = 0.5,
          shape = "plaintext")
    }
  }

  # Define the graph footer
  if (!is.null(footer)){

    heading_levels <- nchar(gsub("^(#*).*", "\\1", footer))

    if (any(heading_levels > 0)){
      footer_fontsize <- vector(mode = "numeric")
      for (i in 1:length(heading_levels)){
        if (heading_levels[i] == 0) footer_fontsize[i] <- 16
        if (heading_levels[i] == 1) footer_fontsize[i] <- 32
        if (heading_levels[i] == 2) footer_fontsize[i] <- 24
        if (heading_levels[i] == 3) footer_fontsize[i] <- 19
        if (heading_levels[i] == 4) footer_fontsize[i] <- 16
        if (heading_levels[i] == 5) footer_fontsize[i] <- 13
        if (heading_levels[i] == 6) footer_fontsize[i] <- 11
      }

      footer <- gsub("^(#| )*(.*)", "\\2", footer)
    }

    footer_node <-
      create_nodes(
        nodes = "footer",
        type = "graph_component",
        label = paste0(footer[1], "\\l"),
        graph_component = "footer_left",
        labelloc = "b",
        x = x_span/2,
        y = -0.7,
        fontname = "Helvetica",
        fontsize = footer_fontsize[1],
        fontcolor = "gray20",
        width = x_span * 1.15,
        height = 0.5,
        shape = "plaintext")

    if (length(footer) > 1){
      footer_right_node <-
        create_nodes(
          nodes = "footer_right",
          type = "graph_component",
          label = paste0(footer[2], "\\r"),
          graph_component = "footer_right",
          labelloc = "b",
          x = x_span/2,
          y = -0.7,
          fontname = "Helvetica",
          fontsize = footer_fontsize[2],
          fontcolor = "gray20",
          width = x_span,
          height = 0.5,
          shape = "plaintext")
    }
  }

  # Define the graph legend
  if (include_legend == TRUE){
    if (!is.null(series_pts)){
      graph_legend_nodes <-
        create_nodes(
          nodes = paste0(unique(series_pts$type), "_node_legend"),
          type = "graph_component",
          label = " ",
          graph_component = "legend_markers",
          x = x_span - legend_offset[1],
          y = y_span -
            seq(0, (length(unique(series_pts$type)) - 1) * 5/x_span, 5/x_span) -
            legend_offset[2],
          color = unique(series_pts$color),
          shape = unique(series_pts$shape),
          penwidth = 2.5,
          width = 0.25,
          height = 0.25)

      graph_legend_node_labels <-
        create_nodes(
          nodes = paste0(unique(series_pts$type), "_node_label_legend"),
          type = "graph_component",
          label = paste0(unique(series_pts$type), "\\r"),
          graph_component = "legend_labels",
          x = x_span/2 - 0.25,
          y = y_span -
            seq(0, (length(unique(series_pts$type)) - 1) * 5/x_span, 5/x_span) -
            legend_offset[2],
          fontname = "Helvetica",
          fontsize = 18,
          fontcolor = "gray15",
          width = x_span,
          height = 0.5,
          shape = "plaintext")
    }
  }

  # Define the x-axis span
  x_axis_nodes <-
    create_nodes(
      nodes = c("x0", "xn"),
      type = "graph_component",
      label = " ",
      graph_component = "x_axis_span",
      x = c(0, x_span),
      y = 0,
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define the y-axis span
  y_axis_nodes <-
    create_nodes(
      nodes = c("y0", "yn"),
      type = "graph_component",
      label = " ",
      graph_component = "y_axis_span",
      x = 0,
      y = c(0, y_span),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define the x-axis line
  x_axis_edge <-
    create_edges(
      from = "x0",
      to = "xn",
      graph_component = "x_axis_line",
      penwidth = 1.0,
      color = "gray",
      arrowhead = "none")

  # Define the y-axis line
  y_axis_edge <-
    create_edges(
      from = "y0",
      to = "yn",
      graph_component = "y_axis_line",
      penwidth = 1.0,
      color = "gray",
      arrowhead = "none")

  # Define the x-axis name
  if (!is.null(x_name)){
    if (x_name_location == "inside"){
      x_axis_name <-
        create_nodes(
          nodes = "x_axis_name",
          type = "graph_component",
          label = paste0(x_name, "\\r"),
          graph_component = "x_axis_name_inside",
          fontname = "Helvetica",
          fontsize = 14,
          fontcolor = "gray",
          x = x_span/2,
          y = 0.3,
          width = x_span,
          height = 0.01,
          shape = "plaintext")

    } else if (x_name_location == "outside"){
      x_axis_name <-
        create_nodes(
          nodes = "x_axis_name",
          type = "graph_component",
          label = x_name,
          graph_component = "x_axis_name_outside",
          fontname = "Helvetica",
          fontsize = 14,
          fontcolor = "gray",
          x = x_span/2,
          y = 0 - 0.6,
          width = 0.01,
          height = 0.01,
          shape = "plaintext")
    }
  }

  # Define the y-axis name
  if (!is.null(y_name)){
    y_axis_name <-
      create_nodes(
        nodes = "y_axis_name",
        type = "graph_component",
        label = paste0("    ", y_name, " \\l"),
        graph_component = "y_axis_name",
        fontname = "Helvetica",
        fontsize = 14,
        fontcolor = "gray",
        x = x_span/2,
        y = y_span,
        width = x_span,
        height = 0.1,
        color = "#FFFFFF00",
        shape = "rectangle")
  }

  # Define an NDF that contains
  # the x-axis tick marks
  if (x_value_labels != "date"){
    x_axis_tick_nodes <-
      create_nodes(
        nodes = c(paste0("x_tick_l-", 0:xy_major_steps[1]),
                  paste0("x_tick_u-", 0:xy_major_steps[1])),
        type = "graph_component",
        label = " ",
        graph_component = "x_axis_major_ticks",
        x = rep(seq(0, x_span, ((x_span - 0) / xy_major_steps[1])), 2),
        y = c(rep(0 - ifelse(x_tick_marks %in% c("centered", "outside"), xy_axis_tick_width[1], 0),
                  xy_major_steps[1] + 1),
              rep(0 + ifelse(x_tick_marks %in% c("centered", "inside"), xy_axis_tick_width[1], 0),
                  xy_major_steps[1] + 1)),
        width = 0.01,
        height = 0.01,
        shape = "plaintext")

    # Define an EDF that draws the
    # x-axis tick marks
    x_axis_tick_edges <-
      create_edges(
        from = paste0("x_tick_l-", 0:xy_major_steps[1]),
        to =   paste0("x_tick_u-", 0:xy_major_steps[1]),
        graph_component = "x_axis_major_tick_lines",
        color = "gray",
        penwidth = 1.0,
        arrowhead = "none")

  } else if (x_value_labels == "date"){

    x_axis_tick_nodes <-
      create_nodes(
        nodes = c(paste0("x_tick-l-", 1:(length(axis_defs$major_tick_fractional))),
                  paste0("x_tick-u-", 1:(length(axis_defs$major_tick_fractional)))),
        type = "graph_component",
        label = " ",
        x = c(axis_defs$major_tick_fractional,
              axis_defs$major_tick_fractional) * 10,
        y = c(rep(0 - ifelse(x_tick_marks %in% c("centered", "outside"), xy_axis_tick_width[1], 0),
                  length(axis_defs$major_tick_fractional)),
              rep(0 + ifelse(x_tick_marks %in% c("centered", "inside"), xy_axis_tick_width[1], 0),
                  length(axis_defs$major_tick_fractional))),
        height = 0.05,
        width = 0.05,
        shape = "plaintext")

    # Define an EDF that draws the
    # x-axis tick marks
    x_axis_tick_edges <-
      create_edges(
        from = paste0("x_tick-l-", 1:(length(axis_defs$major_tick_fractional))),
        to =   paste0("x_tick-u-", 1:(length(axis_defs$major_tick_fractional))),
        graph_component = "x_axis_major_tick_lines",
        color = "gray",
        penwidth = 1.0,
        arrowhead = "none")
  }

  # Define an NDF that contains
  # the x-axis major grid lines
  if (x_value_labels != "date"){
    x_axis_major_grid_nodes <-
      create_nodes(
        nodes = c(paste0("x_maj_grid_l-", 1:xy_major_steps[1]),
                  paste0("x_maj_grid_u-", 1:xy_major_steps[1])),
        type = "graph_component",
        label = " ",
        graph_component = "x_axis_major_grid",
        x = rep(seq(0, x_span, ((x_span - 0) / xy_major_steps[1]))[-1], 2),
        y = c(rep(0, xy_major_steps[1] + 1)[-1],
              rep(y_span, xy_major_steps[1] + 1)[-1]),
        width = 0.01,
        height = 0.01,
        shape = "plaintext")

  } else if (x_value_labels == "date"){

    x_axis_major_grid_nodes <-
      create_nodes(
        nodes = c(paste0("x_maj_grid_l-", 1:(length(axis_defs$major_tick_fractional))),
                  paste0("x_maj_grid_u-", 1:(length(axis_defs$major_tick_fractional)))),
        type = "graph_component",
        label = " ",
        x = c(axis_defs$major_tick_fractional,
              axis_defs$major_tick_fractional) * 10,
        y = c(rep(0, length(axis_defs$major_tick_fractional)),
              rep(y_span, length(axis_defs$major_tick_fractional))),
        height = 0.05,
        width = 0.05,
        shape = "plaintext")
  }

  # Define an EDF that draws the
  # x-axis major grid lines

  if (x_value_labels != "date"){
    x_axis_major_grid_edges <-
      create_edges(
        from = paste0("x_maj_grid_l-", 1:xy_major_steps[1]),
        to =   paste0("x_maj_grid_u-", 1:xy_major_steps[1]),
        graph_component = "x_axis_major_grid_lines",
        penwidth = 1.0,
        color = "gray95",
        arrowhead = "none")
  } else if (x_value_labels == "date"){
    x_axis_major_grid_edges <-
      create_edges(
        from = paste0("x_maj_grid_l-", 1:(length(axis_defs$major_tick_fractional))),
        to =   paste0("x_maj_grid_u-", 1:(length(axis_defs$major_tick_fractional))),
        graph_component = "x_axis_major_grid_lines",
        penwidth = 1.0,
        color = "gray95",
        arrowhead = "none")
  }

  # Define an NDF that contains
  # the y-axis major grid lines
  y_axis_major_grid_nodes <-
    create_nodes(
      nodes = c(paste0("y_maj_grid_l-", 1:xy_major_steps[2]),
                paste0("y_maj_grid_u-", 1:xy_major_steps[2])),
      type = "graph_component",
      label = " ",
      graph_component = "y_axis_major_grid",
      y = rep(seq(0, y_span, ((y_span - 0) / xy_major_steps[2]))[-1], 2),
      x = c(rep(0, xy_major_steps[2] + 1)[-1],
            rep(y_span, xy_major_steps[2] + 1)[-1]),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis major grid lines
  y_axis_major_grid_edges <-
    create_edges(
      from = paste0("y_maj_grid_l-", 1:xy_major_steps[2]),
      to =   paste0("y_maj_grid_u-", 1:xy_major_steps[2]),
      graph_component = "y_axis_major_grid_lines",
      penwidth = 1.0,
      color = "gray95",
      arrowhead = "none")

  # Define an NDF that contains
  # the y-axis tick marks
  y_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_tick_l-", 0:xy_major_steps[2]),
                paste0("y_tick_u-", 0:xy_major_steps[2])),
      type = "graph_component",
      label = " ",
      graph_component = "y_axis_major_ticks",
      y = rep(seq(0, y_span, ((y_span - 0) / xy_major_steps[2])), 2),
      x = c(rep(0 - ifelse(y_tick_marks %in% c("centered", "outside"),
                           xy_axis_tick_width[2], 0),
                xy_major_steps[2] + 1),
            rep(0 + ifelse(y_tick_marks %in% c("centered", "inside"),
                           xy_axis_tick_width[2], 0),
                xy_major_steps[2] + 1)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick marks
  y_axis_tick_edges <-
    create_edges(
      from = paste0("y_tick_l-", 0:xy_major_steps[2]),
      to =   paste0("y_tick_u-", 0:xy_major_steps[2]),
      graph_component = "y_axis_major_tick_lines",
      penwidth = 1.0,
      color = "gray",
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis minor tick marks
  if (x_value_labels != "date"){

    x_axis_minor_tick_nodes <-
      create_nodes(
        nodes = c(paste0("x_minor_tick_l-", 0:(xy_major_steps[1] * 2)),
                  paste0("x_minor_tick_u-", 0:(xy_major_steps[1] * 2))),
        type = "graph_component",
        label = " ",
        graph_component = "x_axis_minor_ticks",
        x = rep(seq(0, x_span, ((x_span - 0) / (xy_major_steps[1] * 2))), 2),
        y = c(rep(0 - ifelse(x_tick_marks %in% c("centered", "outside"),
                             xy_axis_tick_width[1]/2, 0),
                  ((xy_major_steps[1] + 1) * 2)),
              rep(0 + ifelse(x_tick_marks %in% c("centered", "inside"),
                             xy_axis_tick_width[1]/2, 0),
                  ((xy_major_steps[1] + 1) * 2))),
        width = 0.01,
        height = 0.01,
        shape = "plaintext")

    # Define an EDF that draws the
    # x-axis tick marks
    x_axis_minor_tick_edges <-
      create_edges(
        from = paste0("x_minor_tick_l-", 0:(xy_major_steps[1] * 2)),
        to =   paste0("x_minor_tick_u-", 0:(xy_major_steps[1] * 2)),
        graph_component = "x_axis_minor_tick_lines",
        penwidth = 1.0,
        color = "gray",
        arrowhead = "none")

  } else if (x_value_labels != "date"){

    if (!is.null(axis_defs$minor_tick_fractional)){

      x_axis_minor_tick_nodes <-
        create_nodes(
          nodes = c(paste0("x_minor_tick_l-", 1:(length(axis_defs$minor_tick_fractional))),
                    paste0("x_minor_tick_u-", 1:(length(axis_defs$minor_tick_fractional)))),
          type = "graph_component",
          label = " ",
          graph_component = "x_axis_minor_ticks",
          x = c(axis_defs$minor_tick_fractional * x_span,
                axis_defs$minor_tick_fractional * x_span),
          y = c(rep(0 - ifelse(x_tick_marks %in% c("centered", "outside"),
                               xy_axis_tick_width[1]/2, 0),
                    length(axis_defs$minor_tick_fractional) * 2),
                rep(0 + ifelse(x_tick_marks %in% c("centered", "inside"),
                               xy_axis_tick_width[1]/2, 0),
                    length(axis_defs$minor_tick_fractional) * 2)),
          width = 0.01,
          height = 0.01,
          shape = "plaintext")

      # Define an EDF that draws the
      # x-axis tick marks
      x_axis_minor_tick_edges <-
        create_edges(
          from = paste0("x_minor_tick_l-", 1:(length(axis_defs$minor_tick_fractional))),
          to =   paste0("x_minor_tick_u-", 1:(length(axis_defs$minor_tick_fractional))),
          graph_component = "x_axis_minor_tick_lines",
          penwidth = 1.0,
          color = "gray",
          arrowhead = "none")
    }
  }

  # Define an NDF that contains
  # the y-axis minor tick marks
  y_axis_minor_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_minor_tick_l-", 0:(xy_major_steps[2] * 2)),
                paste0("y_minor_tick_u-", 0:(xy_major_steps[2] * 2))),
      type = "graph_component",
      label = " ",
      graph_component = "y_axis_minor_ticks",
      y = rep(seq(0, y_span, ((y_span - 0) / (xy_major_steps[2] * 2))), 2),
      x = c(rep(0 - ifelse(x_tick_marks %in% c("centered", "outside"),
                           xy_axis_tick_width[2]/2, 0),
                ((xy_major_steps[2] + 1) * 2)),
            rep(0 + ifelse(x_tick_marks %in% c("centered", "inside"),
                           xy_axis_tick_width[2]/2, 0),
                ((xy_major_steps[2] + 1) * 2))),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick minor marks
  y_axis_minor_tick_edges <-
    create_edges(
      from = paste0("y_minor_tick_l-", 0:(xy_major_steps[2] * 2)),
      to =   paste0("y_minor_tick_u-", 0:(xy_major_steps[2] * 2)),
      graph_component = "y_axis_minor_tick_lines",
      penwidth = 1.0,
      color = "gray",
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis labels
  if (x_value_labels != "date"){
    x_axis_labels <-
      create_nodes(
        nodes = paste0("xlab-", 0:xy_major_steps[1]),
        type = "graph_component",
        label = x_labels,
        graph_component = "x_axis_labels",
        x = seq(0, x_span, ((x_span - 0) / xy_major_steps[1])),
        y = -0.3 - xy_axis_lab_dist[1],
        shape = "plaintext",
        fontname = "Helvetica",
        fontcolor = "gray")

    if (include_xy_minima[1] == FALSE){
      x_axis_labels[1, which(colnames(x_axis_labels) == "fontcolor")] <- "#FFFFFF00"
    }

  } else if (x_value_labels == "date"){

    x_axis_labels <-
      create_nodes(
        nodes = paste0("xlab-", 1:length(axis_defs$tick_label_locations_fractional)),
        type = "graph_component",
        label = x_labels,
        graph_component = "x_axis_labels",
        x = axis_defs$tick_label_locations_fractional * x_span,
        y = -0.3 - xy_axis_lab_dist[1],
        labelloc = "t",
        shape = "plaintext",
        fontname = "Helvetica",
        fontcolor = "gray")
  }

  # Define an NDF that contains
  # the y-axis labels
  y_axis_labels <-
    create_nodes(
      nodes = paste0("ylab-", 0:xy_major_steps[2]),
      type = "graph_component",
      label = paste0(y_labels, "\\r"),
      graph_component = "y_axis_labels",
      x = -0.6 - xy_axis_lab_dist[2],
      y = seq(0, y_span, ((y_span - 0) / xy_major_steps[2])),
      shape = "plaintext",
      fontname = "Helvetica",
      fontcolor = "gray")

  if (include_xy_minima[2] == FALSE){
    y_axis_labels[1, which(colnames(y_axis_labels) == "fontcolor")] <- "#FFFFFF00"
  }

  # Combine all NDFs for the chart components
  chart_component_nodes <-
    combine_nodes(
      x_axis_nodes, y_axis_nodes,
      x_axis_labels, y_axis_labels,
      x_axis_major_grid_nodes,
      y_axis_major_grid_nodes,
      x_axis_tick_nodes, y_axis_tick_nodes)

  if (exists("x_axis_minor_tick_nodes")){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    x_axis_minor_tick_nodes)
  }

  if (exists("y_axis_minor_tick_nodes")){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    y_axis_minor_tick_nodes)
  }

  if (exists("graph_legend_nodes") & exists("graph_legend_node_labels")){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    graph_legend_nodes,
                    graph_legend_node_labels)
  }

  if (!is.null(x_name)){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    x_axis_name)
  }

  if (!is.null(y_name)){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    y_axis_name)
  }

  if (!is.null(heading)){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    heading_node)
  }

  if (!is.null(heading) & length(heading) > 1){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    heading_right_node)
  }

  if (!is.null(footer)){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    footer_node)
  }

  if (!is.null(footer) & length(footer) > 1){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    footer_right_node)
  }

  if (include_legend == TRUE){
    if (!is.null(series_pts)){
      chart_component_nodes <-
        combine_nodes(chart_component_nodes,
                      graph_legend_nodes)
      chart_component_nodes <-
        combine_nodes(chart_component_nodes,
                      graph_legend_node_labels)
    }
  }

  # Combine all EDFs for the chart components
  chart_component_edges <-
    combine_edges(
      x_axis_edge, y_axis_edge,
      x_axis_major_grid_edges,
      y_axis_major_grid_edges,
      x_axis_tick_edges,
      y_axis_tick_edges)

  if (exists("x_axis_minor_tick_edges")){
    chart_component_edges <-
      combine_edges(chart_component_edges,
                    x_axis_minor_tick_edges)
  }

  if (exists("y_axis_minor_tick_edges")){
    chart_component_edges <-
      combine_edges(chart_component_edges,
                    y_axis_minor_tick_edges)
  }

  # Combine the finalized NDFs and EDFs for the
  # graph components and create a graph object
  # with global attributes set
  graph_components <-
    create_graph(nodes_df = chart_component_nodes,
                 edges_df = chart_component_edges)

  graph_components <-
    set_global_graph_attr(graph_components,
                          "graph", "layout", "neato")

  # Set the color for the background of the entire
  # viewable area
  graph_components <-
    set_global_graph_attr(graph_components,
                          "graph", "bgcolor", bg_color)

  graph_components <-
    set_global_graph_attr(graph_components,
                          "graph", "ratio", "1")

  graph_components <-
    set_global_graph_attr(graph_components,
                          "node", "fontname", "Helvetica")

  graph_components <-
    set_global_graph_attr(graph_components,
                          "node", "fixedsize", "true")

  graph_components <-
    set_global_graph_attr(graph_components,
                          "node", "margin", "0.0")

  graph_components <-
    set_global_graph_attr(graph_components,
                          "graph", "ratio",
                          aspect_ratio[2]/
                            aspect_ratio[1])

  # Add the (x, y) points
  if (!is.null(series_pts)){
    graph_with_data <-
      add_node_df(graph_components, series_pts)
  }

  if (!is.null(series_pts)){
    graph_with_data <-
      add_node_df(graph_components, series_pts)
  }

  if (!is.null(series_x_date_pts)){
    graph_with_data <-
      add_node_df(graph_components, series_x_date_pts)
  }

  if (!is.null(series_y_date_pts)){
    graph_with_data <-
      add_node_df(graph_components, series_y_date_pts)
  }

  # Add the lines between (x, y) points
  if (!is.null(series_lines)){
    graph_with_data <-
      add_edge_df(graph_with_data, series_lines)
  }

  if (!exists("graph_with_data")){
    return(graph_components)
  } else{
    return(graph_with_data)
  }
}
