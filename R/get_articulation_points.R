#' Get articulation points
#'
#' @description
#'
#' Get the nodes in the graph that are identified as articulation points.
#'
#' @inheritParams render_graph
#'
#' @return a vector of node IDs.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 12,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = shape,
#'     values = "square")
#'
#' # Get the articulation points
#' # in the graph (i.e., those
#' # nodes that if any were to be
#' # removed, the graph would
#' # become disconnected)
#' graph %>%
#'   get_articulation_points()
#'
#' # For the articulation points,
#' # change the node shape to
#' # a `circle`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = get_articulation_points(.)) %>%
#'   set_node_attrs_ws(
#'     node_attr = shape,
#'     value = "circle")
#'
#' @export
get_articulation_points <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the graph's articulation points
  articulation_points <-
    igraph::articulation_points(ig_graph)

  articulation_points <-
    igraph::as_ids(articulation_points)

  sort(as.integer(articulation_points))
}
