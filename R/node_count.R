#' Get count of all nodes in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a numeric vector of single length.
#' @examples
#' # Set a seed
#' set.seed(23)
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
#' # Get a count of nodes in the graph
#' node_count(graph)
#' #> 26
#' @export node_count

node_count <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0)
  }

  nrow(graph$nodes_df)
}
