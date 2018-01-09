#' Get non-neighbors of a node in a graph
#' @description Get the set of all nodes not
#' neighboring a single graph node.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node a single-length vector containing a
#' node ID value.
#' @return a vector of node ID values.
#' @examples
#' # Create a simple, directed graph with 5
#' # nodes and 4 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Find all non-neighbors of node `2`
#' graph %>%
#'   get_non_nbrs(node = 2)
#' #> [1] 4 5
#' @export get_non_nbrs

get_non_nbrs <- function(graph,
                         node) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Get predecessors and successors for the `node`
  node_nbrs <-
    c(get_predecessors(graph, node = node),
      get_successors(graph, node = node))

  # Get all non-neighbors to the `node`
  node_non_nbrs <-
    base::setdiff(
      base::setdiff(get_node_ids(graph), node),
      node_nbrs)

  # Get a unique set of node ID values
  node_non_nbrs <- sort(unique(node_non_nbrs))

  # If there are no non-neighbors, then return `NA`
  if (length(node_non_nbrs) == 0) {
    return(NA)
  } else {
    return(node_non_nbrs)
  }
}
