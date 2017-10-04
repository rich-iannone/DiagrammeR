#' Get count of all unconnected nodes in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of nodes in the
#' graph that are not connected to any other node.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a numeric vector of single length.
#' @examples
#' # Create a graph with a
#' # path of nodes and 3
#' # unconnected nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   add_n_nodes(n = 3)
#'
#' # Get a count of all nodes
#' # in the graph
#' graph %>%
#'   count_nodes()
#' #> [1] 6
#'
#' # Get a count of all
#' # unconnected nodes in the
#' # graph
#' graph %>%
#'  count_unconnected_nodes()
#' #> [1] 3
#' @export count_unconnected_nodes

count_unconnected_nodes <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0)
  }

  # Create bindings for specific variables
  id <- from <- to <- node_id <- NULL

  # Get tbl with all nodes that are part
  # of edges
  nodes_in_edf <-
    dplyr::bind_rows(
      graph$edges_df %>%
        dplyr::select(from) %>%
        dplyr::rename(node_id = from),
      graph$edges_df %>%
        dplyr::select(to) %>%
        dplyr::rename(node_id = to)) %>%
    dplyr::distinct()

  # Get tbl with all nodes that are
  # in the node data frame
  nodes_in_ndf <-
    graph$nodes_df %>%
    dplyr::select(id) %>%
    rename(node_id = id)

  # Get nodes not in edge definitions
  nodes_not_in_edf <-
    dplyr::setdiff(
      nodes_in_ndf,
      nodes_in_edf) %>%
    dplyr::pull(node_id)

  if (length(nodes_in_edf > 0)) {
    return(length(nodes_not_in_edf))
  } else {
    return(0)
  }
}
