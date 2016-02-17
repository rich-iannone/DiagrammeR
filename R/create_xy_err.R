#' Create a series of (x, y) error values for a graph
#' @description Create a series of (x, y) error values
#' along for associated (x, y) data points in a series
#' for plotting on a graph.
#' @param xy_pts an NDF containing (x, y) for graphing
#' @param x_err a vector of error values for associated
#' x values.
#' @param y_err a vector of error values for associated
#' y values.
#' @param line_width the line width for the error lines.
#' @param line_color the color of error lines.
#' @param x_err_pos a vector of upper error values for
#' associated x values.
#' @param x_err_neg a vector of lower error values for
#' associated x values.
#' @param y_err_pos a vector of upper error values for
#' associated y values.
#' @param y_err_neg a vector of lower error values for
#' associated y values.
#' @export create_xy_err

create_xy_err <- function(xy_pts,
                          x_err = NULL,
                          y_err = NULL,
                          line_width = 1.5,
                          line_color = "gray50",
                          x_err_pos = NULL,
                          x_err_neg = NULL,
                          y_err_pos = NULL,
                          y_err_neg = NULL){

  x_err_count <- length(x_err)
  y_err_count <- length(y_err)

  # Create NDFs that contain sets
  # of (x, y) points alongside attributes
  chart_error_nodes_x <-
    create_nodes(
      nodes = c(paste0("x_err_upper_", xy_pts$type, "_", 1:x_err_count),
                paste0("x_err_lower_", xy_pts$type, "_", 1:x_err_count)),
      type = c(xy_pts$type, xy_pts$type),
      label = " ",
      graph_component = "xy_err",
      x = c(xy_pts$x + x_err, xy_pts$x - x_err),
      y = c(xy_pts$y, xy_pts$y),
      shape = "plaintext",
      fixedsize = "true",
      width = 0.01,
      height = 0.01
    )

  chart_error_nodes_y <-
    create_nodes(
      nodes = c(paste0("y_err_upper_", xy_pts$type, "_", 1:y_err_count),
                paste0("y_err_lower_", xy_pts$type, "_", 1:y_err_count)),
      type = c(xy_pts$type, xy_pts$type),
      label = " ",
      graph_component = "xy_err",
      y = c(xy_pts$y + y_err, xy_pts$y - y_err),
      x = c(xy_pts$x, xy_pts$x),
      shape = "plaintext",
      fixedsize = "true",
      width = 0.01,
      height = 0.01
    )

  chart_error_nodes_xy <-
    combine_nodes(
      chart_error_nodes_x,
      chart_error_nodes_y
    )

  # Create EDFs that contain sets
  # of lines alongside attributes
  chart_error_lines_x <-
    create_edges(
      from = chart_error_nodes_x$nodes[1:x_err_count],
      to = chart_error_nodes_x$nodes[(1 + x_err_count):(x_err_count * 2)],
      arrowhead = "none",
      penwidth = line_width,
      color = line_color)

  chart_error_lines_y <-
    create_edges(
      from = chart_error_nodes_y$nodes[1:y_err_count],
      to = chart_error_nodes_y$nodes[(1 + y_err_count):(y_err_count * 2)],
      arrowhead = "none",
      penwidth = line_width,
      color = line_color)

  chart_error_lines_xy <-
    combine_edges(
      chart_error_lines_x,
      chart_error_lines_y
    )

  # Incorporate the NDF and EDF into a
  # list object
  chart_error <-
    list(chart_error_nodes_xy = chart_error_nodes_xy,
         chart_error_lines_xy = chart_error_lines_xy)

  attr(chart_error, "class") <- "dgr_graph_xy_err"
  return(chart_error)
}
