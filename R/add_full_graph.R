#' Add a fully connected graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a fully connected graph either
#' with or without loops. If the graph object set as
#' directed, the added graph will have edges to and from
#' each pair of nodes. In the undirected case, a single
#' edge will link each pair of nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of nodes comprising the fully
#' connected graph.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} or \code{NULL} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' connected graph.
#' @param edge_wt_matrix an optional matrix of \code{n}
#' by \code{n} dimensions containing values to apply
#' as edge weights.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_full_graph

add_full_graph <- function(graph,
                           n,
                           type = NULL,
                           label = TRUE,
                           rel = NULL,
                           edge_wt_matrix = NULL,
                           keep_loops = FALSE) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Create initial adjacency matrix based
  adj_matrix <- matrix(1, nc = n, nr = n)

  # Remove loops by making the diagonal of the
  # adjacency matrix all 0
  if (keep_loops == FALSE) {
    adj_matrix <-
      adj_matrix -
      diag(1, nrow = nrow(adj_matrix), ncol = ncol(adj_matrix))
  }

  if (is_graph_directed(graph)) {

    # Create a new directed graph based on the
    # adjacency matrix `adj_matrix`
    new_graph <-
      from_adj_matrix(adj_matrix, mode = "directed")

    # If a matrix of edge weights provided, apply those
    # to each of the edges in a row-major fashion
    if (!is.null(edge_wt_matrix)) {

      new_graph <-
        set_edge_attrs(
          new_graph,
          edge_attr = "weight",
          values = as.numeric(edge_wt_matrix)[
            which(as.numeric(adj_matrix) == 1)])
    }
  } else if (is_graph_directed == FALSE) {

    new_graph <-
      from_adj_matrix(adj_matrix,
                      mode = "undirected")

    # If a matrix of edge weights provided, apply those
    # from the bottom triangle to each of the edges in a
    # row-major fashion
    if (!is.null(edge_wt_matrix)) {

      new_graph <-
        set_edge_attrs(
          new_graph,
          edge_attr = "weight",
          values = edge_wt_matrix[
            lower.tri(
              edge_wt_matrix,
              diag = ifelse(keep_loops == FALSE,
                            FALSE, TRUE))])
    }
  }

  # Add label values to nodes
  if (label == TRUE) {
    new_graph$nodes_df[, 3] <- new_graph$nodes_df[, 1]
  } else if (!is.null(label) &
             label != FALSE) {
    if (length(label) == nrow(new_graph)) {
      new_graph$nodes_df[, 3] <- label
    }
  }

  # Add type value to all new nodes
  if (!is.null(type) &
      length(type) == 1) {
    new_graph$nodes_df[, 2] <- type
  }

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    graph <- combine_graphs(graph, new_graph)

    # Update the `last_node` counter
    graph$last_node <- nodes_created + n

    return(graph)
  } else {
    return(new_graph)
  }
}
