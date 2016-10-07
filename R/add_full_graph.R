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
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' connected graph.
#' @param edge_wt_matrix an optional matrix of \code{n}
#' by \code{n} dimensions containing values to apply
#' as edge weights.
#' @return a graph object of class \code{dgr_graph}.
#' @import igraph graph_from_adjacency_matrix
#' @export add_full_graph

add_full_graph <- function(graph,
                           n,
                           type = NULL,
                           rel = NULL,
                           edge_wt_matrix = NULL,
                           keep_loops = FALSE) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Create initial adjacency matrix based
  adj_matrix <- matrix(1, nc = n, nr = n)

  if (keep_loops == FALSE) {
    adj_matrix <-
      adj_matrix -
      diag(1, nrow = nrow(adj_matrix), ncol = ncol(adj_matrix))
  }

  if (is_graph_directed(graph)) {

    new_graph <-
      from_adj_matrix(adj_matrix, mode = "directed")

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
      from_adj_matrix(adj_matrix, mode = "undirected")

    if (!is.null(edge_wt_matrix)) {

      new_graph <-
        set_edge_attrs(
          new_graph,
          edge_attr = "weight",
          values = edge_wt_matrix[
            lower.tri(edge_wt_matrix, diag = FALSE)])
    }
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
