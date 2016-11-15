#' Get all neighbors of one or more nodes
#' @description With one or more nodes, get the set of
#' all neighboring nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes a vector of node ID values.
#' @return a vector of node ID values.
#' @examples
#' # Create a simple, directed graph with 5
#' # nodes and 4 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(5)
#'
#' # Find all neighbor nodes for node `2`
#' graph %>% get_nbrs(2)
#' #> [1] 1 3
#'
#' # Find all neighbor nodes for nodes `1`
#' # and `5`
#' graph %>% get_nbrs(c(1, 5))
#' #> [1] 2 4
#'
#' # Color node `3` with purple, get its
#' # neighbors and color those nodes green
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(3) %>%
#'   set_node_attrs_ws("color", "purple") %>%
#'   clear_selection() %>%
#'   select_nodes_by_id(get_nbrs(., 3)) %>%
#'   set_node_attrs_ws("color", "green")
#' @export get_nbrs

get_nbrs <- function(graph,
                     nodes) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get predecessors and successors for all nodes
  # in `nodes`
  for (i in 1:length(nodes)) {
    if (i == 1) {
      node_nbrs <- vector(mode = 'numeric')
    }
    node_nbrs <-
      c(node_nbrs,
        c(get_predecessors(graph, node = nodes[i]),
          get_successors(graph, node = nodes[i])))
  }

  # Get a unique set of node ID values
  node_nbrs <- sort(unique(node_nbrs))

  # If there are no neighbors, then return NA
  if (length(node_nbrs) == 0) {
    return(NA)
  } else {
    return(node_nbrs)
  }
}
