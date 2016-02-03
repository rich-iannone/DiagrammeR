#' Create a joining lines for (x, y) points
#' @description Create lines for (x, y) points, along
#' with other style attributes, for the purpose of
#' graph plotting
#' @param xy_pts an NDF containing (x, y) for graphing
#' @param line_width the line width.
#' @param line_color the line color.
#' @export create_xy_lines

create_xy_lines <- function(xy_pts,
                            line_width = 2.5,
                            line_color = "gray"){

  # Get a count of points from the NDF
  point_count <- nrow(xy_pts)

  # Obtain the node ID values
  node_id_values <-
    xy_pts$nodes

  # Create the EDF that contains a set
  # of lines alongside attributes
  chart_lines <-
    create_edges(
      from = node_id_values[1:(point_count-1)],
      to = node_id_values[2:(point_count)],
      arrowhead = "none",
      penwidth = line_width,
      color = line_color)

  return(chart_lines)
}
