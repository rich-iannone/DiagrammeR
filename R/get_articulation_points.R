#' Get articulation points
#' @description Get the nodes in the graph that are
#' identified as articulation points.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector of node IDs.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     30, 50, set_seed = 1) %>%
#'   set_node_attrs("shape", "square")
#'
#' # Get the articulation points in the graph (i.e.,
#' # those nodes that if any were to be removed, the
#' # graph would become disconnected)
#' get_articulation_points(graph)
#' #> [1]  8 22 24
#'
#' # For the articulation points, change the node
#' # shape to a circle
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     get_articulation_points(.)) %>%
#'   set_node_attrs_ws("shape", "circle")
#' @importFrom igraph articulation_points as_ids
#' @export get_articulation_points

get_articulation_points <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the graph's articulation points
  articulation_points <-
    igraph::articulation_points(ig_graph)

  articulation_points <-
    igraph::as_ids(articulation_points)

  return(sort(as.integer(articulation_points)))
}
