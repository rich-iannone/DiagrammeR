#' Delete all selected nodes in a node selection
#' @description In a graph object of class
#' \code{dgr_graph}, delete all nodes present in a
#' selection.
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
#' # Select node with ID `1`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(1)
#'
#' # Delete node in selection (and any attached edges)
#' graph <-
#'   graph %>%
#'   delete_nodes_ws
#'
#' # Get a count of nodes in the graph
#' node_count(graph)
#' #> [1] 2
#' @export delete_nodes_ws

delete_nodes_ws <- function(graph) {

  # If no node selection is available, return
  # the graph unchanged
  if (is.null(graph$selection$nodes)) {
    return(graph)
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get a vector of the nodes to be deleted
  nodes_to_delete <- graph$selection$nodes

  # Delete all nodes in `nodes_to_delete`
  for (i in 1:length(nodes_to_delete)) {

    graph <-
      delete_node(
        graph = graph,
        node = nodes_to_delete[i])
  }

  # Remove all nodes in selection
  graph$selection$nodes <- NULL

  # Update the `last_node` counter
  graph$last_node <- nodes_created

  return(graph)
}
