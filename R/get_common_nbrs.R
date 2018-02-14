#' Get all common neighbors between two or more nodes
#' @description With two or more nodes, get the set of
#' common neighboring nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes a vector of node ID values of length
#' at least 2.
#' @return a vector of node ID values.
#' @examples
#' # Create a directed graph with 5 nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Find all common neighbor nodes
#' # for nodes `1` and `2` (there are no
#' # common neighbors amongst them)
#' graph %>%
#'   get_common_nbrs(
#'     nodes = c(1, 2))
#'
#' # Find all common neighbor nodes for
#' # nodes `1` and `3`
#' graph %>%
#'   get_common_nbrs(
#'     nodes = c(1, 3))
#' @export get_common_nbrs

get_common_nbrs <- function(graph,
                            nodes) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
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
