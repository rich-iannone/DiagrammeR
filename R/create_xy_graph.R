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
#' @export create_xy_graph

create_xy_graph <- function(series_pts,
                            series_lines,
                            x_min,
                            x_max,
                            x_divisions,
                            y_min,
                            y_max,
                            y_divisions,
                            x_axis_lab_dist = 0.3,
                            y_axis_lab_dist = 0.3,
                            x_axis_tick_width = 0.1,
                            y_axis_tick_width = 0.1,
                            color_axis_ticks = "gray",
                            color_axis_labels = "gray"){

  # Define the x-axis span
  x_axis_nodes <-
    create_nodes(
      nodes = c("x0", "xn"),
      label = " ",
      x = c(x_min, x_max + 0.5),
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
      y = c(y_min, y_max + 0.5),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define the x-axis line
  x_axis_edge <-
    create_edges(
      from = "x0",
      to = "xn",
      color = color_axis_ticks)

  # Define the y-axis line
  y_axis_edge <-
    create_edges(
      from = "y0",
      to = "yn",
      color = color_axis_ticks)

  # Define an NDF that contains
  # the x-axis tick marks
  x_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("x_tick_l-", 1:x_divisions),
        paste0("x_tick_u-", 1:x_divisions)),
      label = " ",
      x = rep(seq(x_min, x_max, ((x_max - x_min) / x_divisions ) )[-1], 2),
      y = c(rep(0 - x_axis_tick_width, x_divisions),
            rep(0 + x_axis_tick_width, x_divisions)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # x-axis tick marks
  x_axis_tick_edges <-
    create_edges(
      from = paste0("x_tick_l-", 1:x_divisions),
      to =   paste0("x_tick_u-", 1:x_divisions),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the y-axis tick marks
  y_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_tick_l-", 1:y_divisions),
                paste0("y_tick_u-", 1:y_divisions)),
      label = " ",
      y = rep(seq(y_min, y_max, ((y_max - y_min) / y_divisions ) )[-1], 2),
      x = c(rep(0 - y_axis_tick_width, y_divisions),
            rep(0 + y_axis_tick_width, y_divisions)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick marks
  y_axis_tick_edges <-
    create_edges(
      from = paste0("y_tick_l-", 1:y_divisions),
      to =   paste0("y_tick_u-", 1:y_divisions),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis labels
  x_axis_labels <-
    create_nodes(
      nodes = paste0("xlab-", 1:x_divisions),
      label = seq(x_min, x_max, ((x_max - x_min) / x_divisions ) )[-1],
      x = seq(x_min, x_max, ((x_max - x_min) / x_divisions ) )[-1],
      y = 0 - y_axis_lab_dist,
      type = "x_axis_labels",
      shape = "plaintext",
      fontcolor = color_axis_labels)

  # Define an NDF that contains
  # the y-axis labels
  y_axis_labels <-
    create_nodes(
      nodes = paste0("ylab-", 1:y_divisions),
      label = seq(y_min, y_max, ((y_max - y_min) / y_divisions ) )[-1],
      x = 0 - y_axis_lab_dist,
      y = seq(y_min, y_max, ((y_max - y_min) / y_divisions ) )[-1],
      type = "y_axis_labels",
      shape = "plaintext",
      fontcolor = color_axis_labels)

  # Combine all NDFs for the chart components
  chart_component_nodes <-
    combine_nodes(
      x_axis_nodes, y_axis_nodes,
      x_axis_labels, y_axis_labels,
      x_axis_tick_nodes, y_axis_tick_nodes)

  # Combine all EDFs for the chart components
  chart_component_edges <-
    combine_edges(
      x_axis_edge, y_axis_edge,
      x_axis_tick_edges, y_axis_tick_edges)

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

  # Add the (x, y) points
  graph_with_data <-
    add_node_df(graph_components, series_pts)

  # Add the lines between (x, y) points
  graph_with_data <-
    add_edge_df(graph_with_data, series_lines)

  return(graph_with_data)
}
