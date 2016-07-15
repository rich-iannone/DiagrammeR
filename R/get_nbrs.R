#' Get all neighbors of one or more nodes
#' @description With one or more nodes, get the set of
#' all neighboring nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param nodes a vector of node ID values.
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
#' # Find all neighbor nodes for node `5`
#' random_graph %>% get_nbrs(5)
#' #> [1] "1"  "2"  "12" "18"
#'
#' # Find all neighbor nodes for nodes `5`, `7`,
#' # and `15`
#' random_graph %>% get_nbrs(c(5, 7, 15))
#' #> [1] "1"  "2"  "6"  "11" "12" "18"
#'
#' # Get neighbors for node `11` and add a node
#' # attribute to color the nodes green, then, color
#' # all other nodes light gray
#' random_graph %<>%
#'   select_nodes_by_id(get_nbrs(., 11)) %>%
#'   set_node_attrs_ws('color', 'green') %>%
#'   invert_selection %>%
#'   set_node_attrs_ws('color', 'gray85') %>%
#'   clear_selection
#'
#' # Render the graph to see the change
#' random_graph %>% render_graph
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

  # Determine whether the node ID values are entirely
  # numeric
  node_id_numeric <-
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(node_nbrs)))),
      FALSE, TRUE)

  # If the node ID values are numeric, then apply a
  # numeric sort and reclass as a `character` type
  if (node_id_numeric) {
    node_nbrs <-
      as.character(sort(as.numeric(node_nbrs)))
  }

  # Return the neighbor node ID values
  return(node_nbrs)
}
