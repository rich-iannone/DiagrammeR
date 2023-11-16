#' Get all common neighbors between two or more nodes
#'
#' @description
#'
#' With two or more nodes, get the set of common neighboring nodes.
#'
#' @inheritParams render_graph
#' @param nodes a vector of node ID values of length at least 2.
#'
#' @return a vector of node ID values.
#'
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
#'
#' @export
get_common_nbrs <- function(
    graph,
    nodes
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Get predecessors and successors for all nodes
  # in `nodes`
  for (i in seq_along(nodes)) {
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
  }

  sort(as.integer(common_nbrs))
}
