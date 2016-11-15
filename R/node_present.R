#' Determine whether a specified node is present in an
#' existing graph object
#' @description From a graph object of class
#' \code{dgr_graph}, determine whether a specified node
#' is present.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node a value that may or may not match a node
#' ID in the graph.
#' @return a logical value.
#' @examples
#' # Set a seed
#' set.seed(24)
#'
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 26,
#'     label = TRUE,
#'     type = c(rep("a", 7),
#'              rep("b", 9),
#'              rep("c", 8),
#'              rep("d", 2)))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = sample(1:26, replace = TRUE),
#'     to = sample(1:26, replace = TRUE),
#'     rel = c(rep("rel_a", 7),
#'             rep("rel_b", 9),
#'             rep("rel_c", 8),
#'             rep("rel_d", 2)))
#'
#' # Create a graph using the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Verify that node with ID `34` is not in graph
#' node_present(graph, 34)
#' #> FALSE
#'
#' # Is node with ID 5 in the graph?
#' node_present(graph, 5)
#' #> TRUE
#'
#' # Are all node ID values from `1` to `26`
#' # in the graph?
#' all(sapply(1:26,
#'       function(x) node_present(graph, x)))
#' #> TRUE
#' @export node_present

node_present <- function(graph,
                         node) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Verify that `node` is given as a single value
  node_is_single_value <-
    ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE) {
    stop("Only a single node can be queried using 'node_present'")
  }

  # Determine whether the value corresponds to a
  # node ID in the graph
  if (node_is_single_value) {

    node_is_present <-
      ifelse(node %in% get_node_ids(graph), TRUE, FALSE)

    return(node_is_present)
  }
}
