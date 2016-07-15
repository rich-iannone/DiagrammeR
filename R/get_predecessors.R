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
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_nodes(
#'     nodes = LETTERS)
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edges(
#'     from = sample(LETTERS, replace = TRUE),
#'     to = sample(LETTERS, replace = TRUE))
#'
#' # From the ndf and edf, create a graph object
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges)
#'
#' # Get predecessors for node `Z` in the graph
#' get_predecessors(graph, node = "Z")
#' #> [1] "A" "R" "R"
#'
#' # If there are no predecessors, NA is returned
#' get_predecessors(graph, node = "A")
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
