#' Get the mean distance
#'
#' Get the mean distance of a graph, which is the average path length in the
#' graph. This operates through calculation of the shortest paths between all
#' pairs of nodes.
#'
#' @inheritParams render_graph
#' @return A single numeric value representing the mean distance of the graph.
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the mean distance
#' graph %>%
#'   get_mean_distance()
#'
#' # Create a full graph and then
#' # get the mean distance value
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   get_mean_distance()
#'
#' @export
get_mean_distance <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the maximum eccentricity of the graph
  igraph::mean_distance(ig_graph)
}
