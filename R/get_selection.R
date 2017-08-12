#' Get the current selection available in a graph
#' object
#' @description Get the current selection of nodes or
#' edges from a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector with the current selection of nodes
#' or edges.
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 6) %>%
#'   add_edge(
#'     from = 1,
#'     to = 2) %>%
#'   add_edge(
#'     from = 1,
#'     to = 3) %>%
#'   add_edge(
#'     from = 3,
#'     to = 4) %>%
#'   add_edge(
#'     from = 4,
#'     to = 5) %>%
#'   add_edge(
#'     from = 4,
#'     to = 6)
#'
#' # Select node `4`, then select all nodes a
#' # distance of 1 away from node `4`, and finally
#' # return the selection of nodes as a vector object
#' graph %>%
#'   select_nodes(nodes = 4) %>%
#'   select_nodes_in_neighborhood(
#'     node = 4,
#'     distance = 1) %>%
#'   get_selection()
#' #> [1] 3 4 5 6
#'
#' # Select edges associated with node `4` and return
#' # the selection of edges
#' graph %>%
#'   select_edges_by_node_id(nodes = 4) %>%
#'   get_selection()
#' #> [1] 3 4 5
#' @export get_selection

get_selection <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Determine which type of selection is available
  if (nrow(graph$node_selection) > 0) {
    selection_type <- "node"
  } else if (nrow(graph$edge_selection) > 0) {
    selection_type <- "edge"
  } else {
    selection_type <- NA
  }

  # If there is no selection available, return NA
  if (is.na(selection_type)) {
    return(NA)
  }

  # For a selection of nodes, return a vector of node
  # ID values
  if (selection_type == "node") {
    return(as.integer(sort(graph$node_selection$node)))
  }

  # For a selection of edges, return a vector of edge
  # ID values
  if (selection_type == "edge") {
    return(as.integer(sort(graph$edge_selection$edge)))
  }
}
