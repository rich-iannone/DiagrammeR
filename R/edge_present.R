#' Determine whether a specified edge is present in an
#' existing graph object
#' @description From a graph object of class
#' \code{dgr_graph}, determine whether a directed edge
#' (defined by a pair of node IDs extant in the graph)
#' is present.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param from a node ID from which the edge to be
#' queried is outgoing.
#' @param to a node ID to which the edge to be queried
#' is incoming.
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
#' # Is there any edge between nodes with IDs
#' # `1` and `2`?
#' edge_present(graph, from = 1, to = 2)
#' #> FALSE
#'
#' # Verify that there is an edge between nodes
#' # `18` and `26`
#' edge_present(graph, from = 18, to = 26)
#' #> TRUE
#' @export edge_present

edge_present <- function(graph,
                         from,
                         to) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Verify that each of the values for `from` and
  # `to` are given as a single value
  from_is_single_value <-
    ifelse(length(from) == 1, TRUE, FALSE)
  to_is_single_value <-
    ifelse(length(to) == 1, TRUE, FALSE)

  # Stop function if either node is not a single value
  if (from_is_single_value == FALSE |
      to_is_single_value == FALSE) {
    stop("Only single nodes for 'from' and 'to' should be specified.")
  }

  # Determine whether pair of nodes provided are in
  # the graph
  if (from_is_single_value & to_is_single_value) {
    nodes_available_in_graph <-
      ifelse(all(c(from, to) %in%
                   get_node_ids(graph)), TRUE, FALSE)
  }

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE) {
    stop("The nodes specified are not both present in the graph.")
  }

  # Determine whether a matching edge is available in
  # the graph
  if (nodes_available_in_graph) {
    edge_is_in_graph <-
      ifelse(any(graph$edges_df$from == from &
                   graph$edges_df$to == to),
             TRUE, FALSE)

    return(edge_is_in_graph)
  }
}
