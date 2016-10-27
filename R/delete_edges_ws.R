#' Delete all selected edges in an edge selection
#' @description In a graph object of class
#' \code{dgr_graph}, delete all edges present in a
#' selection.
#'
#' Selections of edges can be performed using
#' the following \code{select_...} functions:
#' \code{select_edges()},
#' \code{select_last_edge()}, or
#' \code{select_edges_by_node_id()}.
#' Selections of edges can also be performed using
#' the following traversal functions:
#' \code{trav_out_edge()}, \code{trav_in_edge()},
#' or \code{trav_both_edge()}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(3) %>%
#'   add_edges_w_string(
#'     "1->3 1->2 2->3")
#'
#' # Select edges attached to node with ID `3` (these
#' # are `1` -> `3` and `2` -> `3`)
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(3)
#'
#' # Delete edges in selection
#' graph <-
#'   graph %>%
#'   delete_edges_ws
#'
#' # Get a count of edges in the graph
#' edge_count(graph)
#' #> [1] 1
#' @export delete_edges_ws

delete_edges_ws <- function(graph) {

  # If no edge selection is available, return the
  # graph unchanged
  if (is.null(graph$selection$edges)) {
    return(graph)
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get vectors of the nodes in edges to be deleted
  from_delete <- graph$selection$edges$from
  to_delete <- graph$selection$edges$to

  # Delete all edges in selection
  for (i in 1:length(from_delete)) {
    graph <-
      delete_edge(
        graph = graph,
        from = from_delete[i],
        to = to_delete[i])
  }

  # If the graph's edf has no rows, ensure that it
  # is set to NULL
  if (!is.null(graph$edges_df)) {
    if (nrow(graph$edges_df) == 0) {

      graph <-
        create_graph(
          nodes_df = graph$nodes_df,
          edges_df = NULL,
          directed = graph$directed,
          graph_attrs = graph$graph_attrs,
          node_attrs = graph$node_attrs,
          edge_attrs = graph$edge_attrs,
          graph_name = graph$graph_name,
          graph_tz = graph$graph_tz,
          graph_time = graph$graph_time)
    }
  }

  # Remove all edges in selection
  graph$selection$edges <- NULL

  # Update the `last_node` counter
  graph$last_node <- nodes_created

  return(graph)
}
