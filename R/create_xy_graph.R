#' Create an (x, y) scatterplot graph
#' @description Create an (x, y) scatterplot graph
#' using an NDF and an EDF for connecting
#' lines.
#' @param series_pts an NDF containing points for
#' graphing.
#' @param series_lines an EDF containing connecting
#' lines.
#' @param x_min the minimum x value to display.
#' @param x_max the maximum x value to display.
#' @param x_divisions the number of divisions between
#' major x-axis marks.
#' @param y_min the minimum y value to display.
#' @param y_max the maximum y value to display.
#' @param y_divisions the number of divisions between
#' major y-axis marks.
#' @param aspect_ratio the aspect ratio of the plot
#' area.
#' @param x_axis_lab_dist the distance between the x
#' axis and its labels.
#' @param y_axis_lab_dist the distance between the y
#' axis and its labels.
#' @param x_axis_tick_width the span of the x-axis tick
#' marks.
#' @param y_axis_tick_width the span of the y-axis tick
#' marks.
#' @param color_axis_ticks the color of the axis tick
#' marks.
#' @param color_axis_labels the color of the axis
#' labels.
#' @examples
#' \dontrun{
#' library(magrittr)
#' # Create a series of (x, y) data
#' # points as an NDF
#' series_01 <-
#'   create_xy_pts(
#'     series_label = "series_01",
#'     x = c(0.0, 1.3, 3.3, 5.2,
#'           6.2, 7.9, 8.6, 9.7),
#'     y = c(2.5, 1.6, 2.7, 3.2,
#'           4.2, 6.9, 8.7, 5.1),
#'     shape = "circle",
#'     line_width = 2.5,
#'     fill_color = "white",
#'     line_color = "blue")
#'
#' # Create an EDF with connections
#' # between (x, y) data points
#' series_01_lines <-
#'   create_xy_lines(
#'     xy_pts = series_01,
#'     line_color = "blue",
#'     line_width = 2.5
#'   )
#'
#' # Create an (x, y) plot
#' xy_plot <-
#' create_xy_graph(
#'   series_pts = series_01,
#'   series_lines = series_01_lines,
#'   x_min = 0, x_max = 10,
#'   x_divisions = 10,
#'   y_min = 0, y_max = 10,
#'   y_divisions = 10)
#'
#' # View the plot
#' xy_plot %>% render_graph
#'
#' # Save the graph as a PDF
#' xy_plot %>% export_graph
#' }
#' @import scales
#' @export create_xy_graph

create_xy_graph <- function(series_pts = NULL,
                            series_lines = NULL,
                            x_min,
                            x_max,
                            x_divisions,
                            y_min,
                            y_max,
                            y_divisions,
                            aspect_ratio = c(1, 1),
                            x_axis_lab_dist = 0.3,
                            y_axis_lab_dist = 0.3,
                            x_axis_tick_width = 0.1,
                            y_axis_tick_width = 0.1,
                            color_axis_ticks = "gray",
                            color_axis_labels = "gray"){

  # Define the x-span and the y-span
  # by the aspect ratio
  x_span <- 10 * aspect_ratio[1]
  y_span <- 10 * aspect_ratio[2]

  # Rescale series data and subset by chart bounds
  if (!is.null(series_pts)){
    series_pts$x <- rescale(series_pts$x, to = c(0, x_span), from = c(x_min, x_max))
    series_pts$y <- rescale(series_pts$y, to = c(0, y_span), from = c(y_min, y_max))
    series_pts <- subset(series_pts, x >= 0)
    series_pts <- subset(series_pts, y >= 0)
    series_pts <- subset(series_pts, x <= x_span)
    series_pts <- subset(series_pts, y <= y_span)
  }

  # Remove extraneous series lines
  if (!is.null(series_lines)){
    series_lines <-
      series_lines[which(series_lines$from %in% series_pts$nodes),]
    series_lines <-
      series_lines[which(series_lines$to %in% series_pts$nodes),]
  }

  # Define the x-axis span
  x_axis_nodes <-
    create_nodes(
      nodes = c("x0", "xn"),
      label = " ",
      x = c(0, x_span),
      y = 0,
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define the y-axis span
  y_axis_nodes <-
    create_nodes(
      nodes = c("y0", "yn"),
      label = " ",
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
      color = color_axis_ticks,
      arrowhead = "none")

  # Define the y-axis line
  y_axis_edge <-
    create_edges(
      from = "y0",
      to = "yn",
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis tick marks
  x_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("x_tick_l-", 0:x_divisions),
                paste0("x_tick_u-", 0:x_divisions)),
      label = " ",
      x = rep(seq(0, x_span, ((x_span - 0) / x_divisions)), 2),
      y = c(rep(0 - x_axis_tick_width, x_divisions + 1),
            rep(0 + x_axis_tick_width, x_divisions + 1)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # x-axis tick marks
  x_axis_tick_edges <-
    create_edges(
      from = paste0("x_tick_l-", 0:x_divisions),
      to =   paste0("x_tick_u-", 0:x_divisions),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the y-axis tick marks
  y_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_tick_l-", 0:y_divisions),
                paste0("y_tick_u-", 0:y_divisions)),
      label = " ",
      y = rep(seq(0, y_span, ((y_span - 0) / y_divisions)), 2),
      x = c(rep(0 - y_axis_tick_width, y_divisions + 1),
            rep(0 + y_axis_tick_width, y_divisions + 1)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick marks
  y_axis_tick_edges <-
    create_edges(
      from = paste0("y_tick_l-", 0:y_divisions),
      to =   paste0("y_tick_u-", 0:y_divisions),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis minor tick marks
  x_axis_minor_tick_nodes <-
    create_nodes(
      nodes = c(paste0("x_minor_tick_l-", 0:(x_divisions * 2)),
                paste0("x_minor_tick_u-", 0:(x_divisions * 2))),
      label = " ",
      x = rep(seq(0, x_span, ((x_span - 0) / (x_divisions * 2))), 2),
      y = c(rep(0 - x_axis_tick_width/2, ((x_divisions + 1) * 2)),
            rep(0 + x_axis_tick_width/2, ((x_divisions + 1) * 2))),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # x-axis tick marks
  x_axis_minor_tick_edges <-
    create_edges(
      from = paste0("x_minor_tick_l-", 0:(x_divisions * 2)),
      to =   paste0("x_minor_tick_u-", 0:(x_divisions * 2)),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the y-axis minor tick marks
  y_axis_minor_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_minor_tick_l-", 0:(y_divisions * 2)),
                paste0("y_minor_tick_u-", 0:(y_divisions * 2))),
      label = " ",
      y = rep(seq(0, y_span, ((y_span - 0) / (y_divisions * 2))), 2),
      x = c(rep(0 - y_axis_tick_width/2, ((y_divisions + 1) * 2)),
            rep(0 + y_axis_tick_width/2, ((y_divisions + 1) * 2))),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick minor marks
  y_axis_minor_tick_edges <-
    create_edges(
      from = paste0("y_minor_tick_l-", 0:(y_divisions * 2)),
      to =   paste0("y_minor_tick_u-", 0:(y_divisions * 2)),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis labels
  x_axis_labels <-
    create_nodes(
      nodes = paste0("xlab-", 0:x_divisions),
      label = seq(x_min, x_max, ((x_max - x_min) / x_divisions)),
      x = seq(0, x_span, ((x_span - 0) / x_divisions)),
      y = 0 - y_axis_lab_dist,
      type = "x_axis_labels",
      shape = "plaintext",
      fontcolor = color_axis_labels)

  # Define an NDF that contains
  # the y-axis labels
  y_axis_labels <-
    create_nodes(
      nodes = paste0("ylab-", 0:y_divisions),
      label = seq(y_min, y_max, ((y_max - y_min) / y_divisions)),
      x = 0 - y_axis_lab_dist,
      y = seq(0, y_span, ((y_span - 0) / y_divisions)),
      type = "y_axis_labels",
      shape = "plaintext",
      fontcolor = color_axis_labels)

  # Combine all NDFs for the chart components
  chart_component_nodes <-
    combine_nodes(
      x_axis_nodes, y_axis_nodes,
      x_axis_labels, y_axis_labels,
      x_axis_tick_nodes, y_axis_tick_nodes,
      x_axis_minor_tick_nodes,
      y_axis_minor_tick_nodes)

  # Combine all EDFs for the chart components
  chart_component_edges <-
    combine_edges(
      x_axis_edge, y_axis_edge,
      x_axis_tick_edges, y_axis_tick_edges,
      x_axis_minor_tick_edges,
      y_axis_minor_tick_edges)

  # Combine the finalized NDFs and EDFs for the
  # graph components and create a graph object
  # with global attributes set
  graph_components <-
    create_graph(nodes_df = chart_component_nodes,
                 edges_df = chart_component_edges)

  graph_components <-
    set_global_graph_attr(graph_components,
                          "graph", "layout", "neato")

  graph_components <-
    set_global_graph_attr(graph_components,
                          "graph", "bgcolor", "transparent")

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
  if (is.null(series_pts)){
    graph_with_data <-
      add_node_df(graph_components, series_pts)
  }

  # Add the lines between (x, y) points
  if (!is.null(series_lines)){
    graph_with_data <-
      add_edge_df(graph_with_data, series_lines)
  }

  return(graph_with_data)
}
