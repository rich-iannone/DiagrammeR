#' Update and display graph object
#' @description Using a \code{dgr_graph} object, update values of counts for nodes, edges, attributes, directed state, and display the schematic in the RStudio Viewer.
#' @param graph a \code{dgr_graph} object, created using the \code{create_graph} function.
#' @param width the width of the graph representation in pixels.
#' @export display_graph_object

display_graph_object <- function(graph, width = 400){

  # Get updated counts of nodes in graph
  total_node_count <- node_count(graph = graph,
                                 type = FALSE)

  # Get updated counts of edges in graph
  if (total_node_count == 0){

    total_edge_count <- 0

  } else {

    total_edge_count <- length(get_edges(graph)[[1]])
  }

  # Get updated counts of graph attributes set
  total_graph_attr <- length(graph$graph_attrs)

  # Get updated counts of node attributes set
  total_node_attr <- length(graph$node_attrs)

  # Get updated counts of edge attributes set
  total_edge_attr <- length(graph$edge_attrs)

  # Create a temporary file
  dir <- tempfile()
  dir.create(dir)

  # Name the temporary file 'index.html'
  htmlFile <- file.path(dir, "index.html")

  html_lines <- readLines(system.file("htmlwidgets/graph_object.html",
                                      package = "DiagrammeR"))

  # Set the node count
  if (total_node_count < 10){
    x_value_nodes <- 209
  } else if (total_node_count < 100){
    x_value_nodes <- 189
  } else if (total_node_count < 1000){
    x_value_nodes <- 169
  } else if (total_node_count < 10000){
    x_value_nodes <- 149
  } else if (total_node_count < 100000){
    x_value_nodes <- 149
  }

  html_lines[65] <- paste0("            <tspan x=\"",
                           x_value_nodes,
                           "\" y=\"269\">",
                           total_node_count,
                           "</tspan>")

  # Set the edge count
  if (total_edge_count < 10){
    x_value_edges <- 421
  } else if (total_edge_count < 100){
    x_value_edges <- 401
  } else if (total_edge_count < 1000){
    x_value_edges <- 381
  } else if (total_edge_count < 10000){
    x_value_edges <- 361
  } else if (total_edge_count < 100000){
    x_value_edges <- 341
  }

  html_lines[62] <- paste0("            <tspan x=\"",
                           x_value_edges,
                           "\" y=\"269\">",
                           total_edge_count,
                           "</tspan>")

  # Set the 'graph_attr' count
  if (total_graph_attr < 10){
    x_value_graph_attr <- 145
  } else if (total_graph_attr < 100){
    x_value_graph_attr <- 135
  } else if (total_graph_attr < 1000){
    x_value_graph_attr <- 125
  }

  html_lines[59] <- paste0("            <tspan x=\"",
                           x_value_graph_attr,
                           "\" y=\"118\">",
                           total_graph_attr,
                           "</tspan>")

  # Set the 'node_attr' count
  if (total_node_attr < 10){
    x_value_node_attr <- 215
  } else if (total_node_attr < 100){
    x_value_node_attr <- 205
  } else if (total_node_attr < 1000){
    x_value_node_attr <- 195
  }

  html_lines[53] <- paste0("            <tspan x=\"",
                           x_value_node_attr,
                           "\" y=\"118\">",
                           total_node_attr,
                           "</tspan>")

  # Set the 'edge_attr' count
  if (total_edge_attr < 10){
    x_value_edge_attr <- 282
  } else if (total_edge_attr < 100){
    x_value_edge_attr <- 272
  } else if (total_edge_attr < 1000){
    x_value_edge_attr <- 262
  }

  html_lines[56] <- paste0("            <tspan x=\"",
                           x_value_edge_attr,
                           "\" y=\"118\">",
                           total_edge_attr,
                           "</tspan>")

  # Modify scaling of image
  html_lines[5] <-
    paste0("<svg width=\"",
           width, "px\" height=\"",
           width, "px\" viewBox=\"0 0 512 512\" version=\"1.1\"",
           " xmlns=\"http://www.w3.org/2000/svg\"",
           " xmlns:xlink=\"http://www.w3.org/1999/xlink\"",
           " xmlns:sketch=\"http://www.bohemiancoding.com/sketch/ns\">")

  if (total_node_count == 0){
    html_lines <-
      c(html_lines[1:51],
        paste0("        <path xmlns=\"http://www.w3.org/2000/svg\" d=\"M212,390 L295,420\" ",
               "id=\"Line\" stroke=\"#99A76A\" stroke-width=\"4\" stroke-linecap=\"square\"",
               " sketch:type=\"MSShapeGroup\" xmlns:sketch=\"http://www.bohemiancoding.com/sketch/ns\"",
               " transform=\"translate(253.500000, 405.000000) scale(-1, -1)",
               " translate(-253.500000, -405.000000) \"/>"),
        html_lines[52:length(html_lines)])
  }

  # Write the 'gv_HTML' file to 'index.html'
  writeLines(html_lines, htmlFile)

  # Display the HTML file in the Viewer
  rstudioapi::viewer(htmlFile)

}
