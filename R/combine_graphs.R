#' Combine two graphs into a single graph
#' @description Combine two graphs in order to make a
#' new graph.
#' @param x a \code{DiagrammeR} graph object to which
#' another graph will be unioned. This graph should be
#' considered the graph from which global graph
#' attributes will be inherited in the resulting graph.
#' @param y a \code{DiagrammeR} graph object that is to
#' be unioned with the graph suppled as \code{x}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a cycle
#' graph_cycle <-
#'  create_graph() %>%
#'    add_cycle(n = 6)
#'
#' # Create a random graph with 8 nodes, 15 edges
#' graph_random <-
#'   create_random_graph(
#'     8, 15, set_seed = 1)
#'
#' # Combine the two graphs in a union operation
#' combined_graph <-
#'   combine_graphs(graph_cycle, graph_random)
#' @importFrom dplyr inner_join rename select
#' @export combine_graphs

combine_graphs <- function(x,
                           y) {

  # Assign NULL to certain variables used as df
  # column names
  new_node_id <- NULL
  type <- NULL
  label <- NULL

  # Get the number of nodes ever created for
  # graph `x`
  nodes_created <- x$last_node

  # Get the node data frame for graph `x`
  x_nodes_df <- get_node_df(x)

  # Get the node data frame for graph `y`
  y_nodes_df <- get_node_df(y)

  # Is label a copy of node IDs in graph `y`?
  if (all(y_nodes_df$nodes == y_nodes_df$label)) {
    y_label_node <- TRUE
  } else {
    y_label_node <- FALSE
  }

  # Add a new node attribute `new_node_id`
  y_nodes_df$new_node_id <-
    seq(nodes_created + 1,
        nodes_created + nrow(y_nodes_df))

  # Get the edge data frame for graph `x`
  x_edges_df <- get_edge_df(x)

  # Get the edge data frame for graph `y`
  y_edges_df <- get_edge_df(y)

  y_edges_df <-
    dplyr::inner_join(
      y_edges_df,
      y_nodes_df,
      by = c("from" = "nodes")) %>%
    dplyr::rename(from_new = new_node_id) %>%
    dplyr::select(-type, -label)

  y_edges_df <-
    dplyr::inner_join(
      y_edges_df,
      y_nodes_df,
      by = c("to" = "nodes")) %>%
    dplyr::rename(to_new = new_node_id) %>%
    dplyr::select(-type, -label)

  # Copy new node IDs to `from` and `to` edge attrs
  y_edges_df$from <- y_edges_df$from_new
  y_edges_df$to <- y_edges_df$to_new

  # Remove last two columns from `y_edges_df`
  y_edges_df <-
    y_edges_df[, -c(ncol(y_edges_df),
                    ncol(y_edges_df) - 1)]

  # Copy new node IDs to `nodes` node attr
  y_nodes_df$nodes <- y_nodes_df$new_node_id

  # Remove the last column from `y_nodes_df`
  y_nodes_df <-
    y_nodes_df[, -ncol(y_nodes_df)]

  # If label is a copy of node ID in graph `y`,
  # rewrite labels to match new node ID values
  if (y_label_node) {
    y_nodes_df$label <- y_nodes_df$nodes
  }

  # Combine the node data frames for both graphs
  combined_nodes <-
    combine_nodes(
      x_nodes_df,
      y_nodes_df)

  # Combine the edge data frames for both graphs
  combined_edges <-
    combine_edges(
      x_edges_df,
      y_edges_df)

  if (is.null(x$dot_code)) {

    dgr_graph <-
      create_graph(
        nodes_df = combined_nodes,
        edges_df = combined_edges,
        graph_attrs = x$graph_attrs,
        node_attrs = x$node_attrs,
        edge_attrs = x$edge_attrs,
        directed = ifelse(
          is_graph_directed(x) == FALSE ||
            is_graph_directed(y) == FALSE,
          FALSE, TRUE),
        graph_name = x$graph_name,
        graph_time = x$graph_time,
        graph_tz = x$graph_tz,
        generate_dot = FALSE)
  }

  if (!is.null(x$dot_code)) {

    dgr_graph <-
      create_graph(
        nodes_df = combined_nodes,
        edges_df = combined_edges,
        graph_attrs = x$graph_attrs,
        node_attrs = x$node_attrs,
        edge_attrs = x$edge_attrs,
        directed = ifelse(
          is_graph_directed(x) == FALSE ||
            is_graph_directed(y) == FALSE,
          FALSE, TRUE),
        graph_name = x$graph_name,
        graph_time = x$graph_time,
        graph_tz = x$graph_tz)
  }

  return(dgr_graph)
}
