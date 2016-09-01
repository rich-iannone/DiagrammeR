#' Get node IDs for predecessor nodes to the specified
#' node
#' @description Provides a vector of node IDs for all
#' nodes that have a
#' connection to the given node.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node a node ID for the selected node.
#' @return a vector of node ID values.
#' @examples
#' # Set a seed
#' set.seed(24)
#'
#' # Create a node data frame (ndf)
#' nodes <- create_nodes(1:26)
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edges(
#'     from = sample(1:26, replace = TRUE),
#'     to = sample(1:26, replace = TRUE))
#'
#' # From the ndf and edf, create a graph object
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges)
#'
#' # Get predecessors for node `26` in the graph
#' get_predecessors(graph, node = 26)
#' #> [1] 18
#'
#' # If there are no predecessors, NA is returned
#' get_predecessors(graph, node = 1)
#' #> [1] NA
#' @export get_predecessors

get_predecessors <- function(graph,
                             node) {

  # Determine whether the graph has any nodes
  graph_is_not_empty <- !is_graph_empty(graph)

  # Determine whether `node` is in the graph
  node_is_in_graph <- node_present(graph, node)

  # Obtain the node's predecessors
  if (graph_is_not_empty &
      node_is_in_graph &
      nrow(edge_info(graph)) > 0) {

    if (length(graph$edges_df[graph$edges_df$to ==
                              node,]$from) == 0) {
      predecessors <- NA
    } else {
      predecessors <-
        graph$edges_df[graph$edges_df$to == node,]$from
    }
    return(predecessors)
  }
}
