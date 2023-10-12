#' Get nodes that form the graph periphery
#'
#' @description
#'
#' Get those nodes that are part of the graph periphery (i.e., have the maximum
#' eccentricity in the graph).
#'
#' @inheritParams render_graph
#'
#' @return A vector of node IDs.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function and
#' # get the nodes in the graph periphery
#' create_graph() %>%
#'   add_gnm_graph(
#'     n = 28,
#'     m = 35,
#'     set_seed = 23) %>%
#'   get_periphery()
#'
#' @export
get_periphery <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Get the eccentricity for each of the graph's nodes
  eccentricity <- get_eccentricity(graph)

  # Get the node ID values for all nodes where the
  # eccentricity is equal to the graph diameter
  # (i.e., maximum eccentricity)
  eccentricity %>%
    dplyr::filter(eccentricity == get_max_eccentricity(graph)) %>%
    dplyr::pull(id) %>%
    as.integer()
}
