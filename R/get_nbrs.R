#' Get all neighbors of one or more nodes
#' @description With one or more nodes, get the set of
#' all neighboring nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param nodes a vector of node ID values.
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
#' # Find all neighbor nodes for node `5`
#' random_graph %>% get_nbrs(5)
#' #> [1]  1  2 12 18
#'
#' # Find all neighbor nodes for nodes `5`, `7`,
#' # and `15`
#' random_graph %>% get_nbrs(c(5, 7, 15))
#' #> [1]  1  2  6 12 18
#'
#' # Color node `11` with purple, get it's
#' # neighbors and color those nodes green
#' random_graph <-
#'   random_graph %>%
#'   select_nodes_by_id(11) %>%
#'   set_node_attrs_ws("color", "purple") %>%
#'   clear_selection %>%
#'   select_nodes_by_id(get_nbrs(., 11)) %>%
#'   set_node_attrs_ws("color", "green")
#'
#' # Render the graph to see the change
#' random_graph %>% render_graph
#'
#' # A node with no neighbors with return `NA`
#' create_graph() %>%
#'   add_node %>%
#'   get_nbrs(1)
#' #> [1] NA
#' @export get_nbrs

get_nbrs <- function(graph,
                     nodes) {

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

  # If there are no neighbors, then return `NA`
  if (length(node_nbrs) == 0) {
    return(NA)
  } else {
    return(node_nbrs)
  }
}
