#' Get non-neighbors of a node in a graph
#' @description Get the set of all nodes not
#' neighboring a single graph node.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node a single-length vector containing a
#' node ID value.
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
#' # Find all non-neighbors of node `5`
#' random_graph %>%
#'   get_non_nbrs(5)
#' #>  [1] "3"  "4"  "6"  "7"  "8"  "9"  "10" "11" "13"
#' #> [12] "14" "15" "16" "17"
#' @export get_non_nbrs

get_non_nbrs <- function(graph,
                         node) {

  # Get predecessors and successors for the `node`
  node_nbrs <-
    c(get_predecessors(graph, node = node),
      get_successors(graph, node = node))

  # Get all non-neighbors to the `node`
  node_non_nbrs <-
    setdiff(
      setdiff(get_nodes(graph), node),
      node_nbrs)

  # Determine whether the node ID values are entirely
  # numeric
  node_id_numeric <-
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(node_non_nbrs)))),
      FALSE, TRUE)

  # If the node ID values are numeric, then apply a
  # numeric sort and reclass as a `character` type
  if (node_id_numeric) {
    node_non_nbrs <-
      as.character(sort(as.numeric(node_non_nbrs)))
  }

  # Return the neighbor node ID values
  return(node_non_nbrs)
}
