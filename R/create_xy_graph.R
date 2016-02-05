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
#' display on the x-axis.
#' @param y_scale the minimum and maximum y value to
#' display on the y-axis.
#' @param xy_major_steps the number of major steps
#' across the x and y directions.
#' @param x_name the x-axis name.
#' @param y_name the y-axis name.
#' @param heading a graph heading placed above the plot
#' area.
#' @param right_heading a right-aligned graph heading
#' placed above the plot area.
#' @param x_name_location the location of the x-axis
#' name, either \code{inside} (default) or \code{outside}.
#' @param xy_axis_lab_dist the distances between the x
#' and y axes and their labels.
#' @param xy_axis_tick_width the widths of the x- and
#' y-axis tick mark
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
#'   create_xy_graph(
#'     series_01,
#'     series_01_lines,
#'     x_name = "x title",
#'     y_name = "y title",
#'     heading = "The graph name",
#'     right_heading = "A right-aligned heading")
#'
#' # View the plot
#' xy_plot %>% render_graph
#'
#' # Save the graph as a PDF
#' xy_plot %>% export_graph
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
                            right_heading = NULL,
                            x_name_location = "inside",
                            xy_axis_lab_dist = c(0.0, 0.0),
                            xy_axis_tick_width = c(0.1, 0.1),
                            color_axis_ticks = "gray",
                            color_axis_labels = "gray"){

  # Take multiple series of points and lines
  pts_lines_df <- list(...)

  for (i in 1:length(pts_lines_df)){
    if (i == 1){
      pts_df <- vector(mode = "numeric")
      lines_df <- vector(mode = "numeric")
    }

    if ("nodes" %in% colnames(pts_lines_df[[i]])){
      pts_df <- c(pts_df, i)
    }

    if ("from" %in% colnames(pts_lines_df[[i]]) &
        "to" %in% colnames(pts_lines_df[[i]])){
      lines_df <- c(lines_df, i)
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

  # If `x_scale`, `y_scale`, and `xy_major_steps` not
  # provided, devise bounds and breaks
  if (is.null(x_scale) &
      is.null(y_scale) &
      is.null(xy_major_steps) &
      !is.null(series_pts)){

    x_scale <-
      c(cbreaks(c(min(series_pts$x),
                  max(series_pts$x)),
                pretty_breaks(10))[[1]][1],
        tail(cbreaks(c(min(series_pts$x),
                       max(series_pts$x)),
                     pretty_breaks(10))[[1]], 1))

    y_scale <-
      c(cbreaks(c(min(series_pts$y),
                  max(series_pts$y)),
                pretty_breaks(10))[[1]][1],
        tail(cbreaks(c(min(series_pts$y),
                       max(series_pts$y)),
                     pretty_breaks(10))[[1]], 1))

    xy_major_steps <-
      c(length(cbreaks(c(min(series_pts$x),
                         max(series_pts$x)),
                       pretty_breaks(10))[[1]]) - 1,
        length(cbreaks(c(min(series_pts$y),
                         max(series_pts$y)),
                       pretty_breaks(10))[[1]]) - 1)
  }

  # Define the x-span and the y-span
  # by the aspect ratio
  x_span <- 10 * aspect_ratio[1]
  y_span <- 10 * aspect_ratio[2]

  # Rescale series data and subset by chart bounds
  if (!is.null(series_pts)){
    series_pts$x <- rescale(series_pts$x, to = c(0, x_span), from = c(x_scale[1], x_scale[2]))
    series_pts$y <- rescale(series_pts$y, to = c(0, y_span), from = c(y_scale[1], y_scale[2]))
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

  # Define the graph heading
  if (!is.null(heading)){
  heading_node <-
    create_nodes(
      nodes = "heading",
      label = paste0(heading, "\\l"),
      x = x_span/2,
      y = 10.4,
      fontsize = 24,
      fontcolor = "gray15",
      width = x_span,
      height = 0.5,
      shape = "plaintext")
  }

  # Define the graph's right-aligned heading
  if (!is.null(right_heading)){
    heading_right_node <-
      create_nodes(
        nodes = "heading_right",
        label = paste0(right_heading, "\\r"),
        x = x_span/2,
        y = 10.4,
        fontsize = 24,
        fontcolor = "gray15",
        width = x_span,
        height = 0.5,
        shape = "plaintext")
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

  # Define the x-axis name
  if (!is.null(x_name)){
    if (x_name_location == "inside"){
      x_axis_name <-
        create_nodes(
          nodes = "x_axis_name",
          label = paste0(x_name, "\\r"),
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
          label = x_name,
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
        label = paste0(y_name, "\\l"),
        fontcolor = "gray",
        fontsize = 14,
        x = 0.6,
        y = y_span,
        shape = "plaintext")
  }

  # Define an NDF that contains
  # the x-axis tick marks
  x_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("x_tick_l-", 0:xy_major_steps[1]),
                paste0("x_tick_u-", 0:xy_major_steps[1])),
      label = " ",
      x = rep(seq(0, x_span, ((x_span - 0) / xy_major_steps[1])), 2),
      y = c(rep(0 - xy_axis_tick_width[1], xy_major_steps[1] + 1),
            rep(0 + xy_axis_tick_width[1], xy_major_steps[1] + 1)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # x-axis tick marks
  x_axis_tick_edges <-
    create_edges(
      from = paste0("x_tick_l-", 0:xy_major_steps[1]),
      to =   paste0("x_tick_u-", 0:xy_major_steps[1]),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the y-axis tick marks
  y_axis_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_tick_l-", 0:xy_major_steps[2]),
                paste0("y_tick_u-", 0:xy_major_steps[2])),
      label = " ",
      y = rep(seq(0, y_span, ((y_span - 0) / xy_major_steps[2])), 2),
      x = c(rep(0 - xy_axis_tick_width[2], xy_major_steps[2] + 1),
            rep(0 + xy_axis_tick_width[2], xy_major_steps[2] + 1)),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick marks
  y_axis_tick_edges <-
    create_edges(
      from = paste0("y_tick_l-", 0:xy_major_steps[2]),
      to =   paste0("y_tick_u-", 0:xy_major_steps[2]),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis minor tick marks
  x_axis_minor_tick_nodes <-
    create_nodes(
      nodes = c(paste0("x_minor_tick_l-", 0:(xy_major_steps[1] * 2)),
                paste0("x_minor_tick_u-", 0:(xy_major_steps[1] * 2))),
      label = " ",
      x = rep(seq(0, x_span, ((x_span - 0) / (xy_major_steps[1] * 2))), 2),
      y = c(rep(0 - xy_axis_tick_width[1]/2, ((xy_major_steps[1] + 1) * 2)),
            rep(0 + xy_axis_tick_width[1]/2, ((xy_major_steps[1] + 1) * 2))),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # x-axis tick marks
  x_axis_minor_tick_edges <-
    create_edges(
      from = paste0("x_minor_tick_l-", 0:(xy_major_steps[1] * 2)),
      to =   paste0("x_minor_tick_u-", 0:(xy_major_steps[1] * 2)),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the y-axis minor tick marks
  y_axis_minor_tick_nodes <-
    create_nodes(
      nodes = c(paste0("y_minor_tick_l-", 0:(xy_major_steps[2] * 2)),
                paste0("y_minor_tick_u-", 0:(xy_major_steps[2] * 2))),
      label = " ",
      y = rep(seq(0, y_span, ((y_span - 0) / (xy_major_steps[2] * 2))), 2),
      x = c(rep(0 - xy_axis_tick_width[2]/2, ((xy_major_steps[2] + 1) * 2)),
            rep(0 + xy_axis_tick_width[2]/2, ((xy_major_steps[2] + 1) * 2))),
      width = 0.01,
      height = 0.01,
      shape = "plaintext")

  # Define an EDF that draws the
  # y-axis tick minor marks
  y_axis_minor_tick_edges <-
    create_edges(
      from = paste0("y_minor_tick_l-", 0:(xy_major_steps[2] * 2)),
      to =   paste0("y_minor_tick_u-", 0:(xy_major_steps[2] * 2)),
      color = color_axis_ticks,
      arrowhead = "none")

  # Define an NDF that contains
  # the x-axis labels
  x_axis_labels <-
    create_nodes(
      nodes = paste0("xlab-", 0:xy_major_steps[1]),
      label = seq(x_scale[1],
                  x_scale[2],
                  ((x_scale[2] - x_scale[1]) / xy_major_steps[1])),
      x = seq(0, x_span, ((x_span - 0) / xy_major_steps[1])),
      y = -0.3 - xy_axis_lab_dist[1],
      type = "x_axis_labels",
      shape = "plaintext",
      fontcolor = color_axis_labels)

  # Define an NDF that contains
  # the y-axis labels
  y_axis_labels <-
    create_nodes(
      nodes = paste0("ylab-", 0:xy_major_steps[2]),
      label = paste0(seq(y_scale[1],
                         y_scale[2],
                         ((y_scale[2] - y_scale[1]) / xy_major_steps[2])),
                     "\\r"),
      x = -0.6 - xy_axis_lab_dist[2],
      y = seq(0, y_span, ((y_span - 0) / xy_major_steps[2])),
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

  if (!is.null(right_heading)){
    chart_component_nodes <-
      combine_nodes(chart_component_nodes,
                    heading_right_node)
  }

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
  if (!is.null(series_pts)){
    graph_with_data <-
      add_node_df(graph_components, series_pts)
  }

  # Add the lines between (x, y) points
  if (!is.null(series_lines)){
    graph_with_data <-
      add_edge_df(graph_with_data, series_lines)
  }

  if (is.null(series_pts) & is.null(series_lines)){
    return(graph_components)
  } else{
    return(graph_with_data)
  }
}
