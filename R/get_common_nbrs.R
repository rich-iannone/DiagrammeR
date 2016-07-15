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
#' library(magrittr)
#'
#' # Create a random, directed graph with 18 nodes
#' # and 22 edges
#' random_graph <-
#'   create_random_graph(
#'     n = 18,
#'     m = 22,
#'     directed = TRUE,
#'     fully_connected = TRUE,
#'     set_seed = 20) %>%
#'   set_global_graph_attrs(
#'     'graph', 'layout', 'sfdp') %>%
#'   set_global_graph_attrs(
#'     'graph', 'overlap', 'false')
#'
#' # Find all common neighbor nodes for nodes `5`
#' # and `7` (there are no common neighbors)
#' random_graph %>%
#'   get_common_nbrs(c(5, 7))
#' #> [1] NA
#'
#' # Find all neighbor nodes for nodes `9` and  `17`
#' random_graph %>%
#'   get_common_nbrs(c(9, 17))
#' #> [1] "1"
#' @export get_common_nbrs

get_common_nbrs <- function(graph,
                            nodes) {

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

  # If there are no intersecting nodes, assign NA to
  # `common_nbrs`
  if (length(common_nbrs) == 0) {
    common_nbrs <- NA
  }

  # If `common_nbrs` has node ID values, determine
  # if the node ID values are numeric and, if so, apply
  # a numeric sort
  if (all(!is.na(common_nbrs))) {

    # Determine whether the node ID values are entirely
    # numeric
    node_id_numeric <-
      ifelse(
        suppressWarnings(
          any(is.na(as.numeric(common_nbrs)))),
        FALSE, TRUE)

    # If the node ID values are numeric, then apply a
    # numeric sort and reclass as a `character` type
    if (node_id_numeric) {
      common_nbrs <-
        as.character(sort(as.numeric(common_nbrs)))
    }
  }

  # Return the common neighbor node ID values
  return(common_nbrs)
}
