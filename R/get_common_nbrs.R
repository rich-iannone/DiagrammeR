#' Get all common neighbors between two or more nodes
#' @description With two or more nodes, get the set of
#' common neighboring nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param nodes a vector of node ID values of length
#' at least 2.
#' @return a vector of node ID values.
#' @examples
#' # Create a random, directed graph with 18 nodes
#' # and 22 edges
#' random_graph <-
#'   create_random_graph(
#'     n = 18,
#'     m = 22,
#'     directed = TRUE,
#'     fully_connected = TRUE,
#'     set_seed = 20)
#'
#' # Find all common neighbor nodes for nodes
#' # `5` and `7` (there are no common neighbors)
#' random_graph %>% get_common_nbrs(c(5, 7))
#' #> [1] NA
#'
#' # Find all neighbor nodes for nodes `9`
#' # and  `17`
#' random_graph %>% get_common_nbrs(c(9, 17))
#' #> [1] 1
#' @export get_common_nbrs

get_common_nbrs <- function(graph,
                            nodes) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get predecessors and successors for all nodes
  # in `nodes`
  for (i in 1:length(nodes)) {
    if (i == 1) {
      nbrs <- list()
    }

    nbrs[[i]] <-
      c(sort(get_predecessors(graph, node = nodes[i])),
        sort(get_successors(graph, node = nodes[i])))
  }

  common_nbrs <- nbrs[[1]]

  for (i in nbrs[-1]) {
    common_nbrs <- intersect(common_nbrs, i)
  }

  if (length(common_nbrs) == 0) {
    return(NA)
  } else {
    return(sort(as.integer(common_nbrs)))
  }
}
