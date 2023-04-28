#' Get the minimum graph eccentricity
#'
#' @description
#'
#' Get the radius of a graph, which is the smallest eccentricity in the graph.
#' The graph eccentricity of a node is its shortest path from the farthest other
#' node in the graph.
#'
#' @inheritParams render_graph
#' @param direction Using `all` (the default), the search will ignore edge
#'   direction while traversing through the graph. With `out`, measurements of
#'   paths will be from a node whereas with `in`, measurements of paths will be
#'   to a node.
#'
#' @return A single numeric value representing the minimum eccentricity of the
#'   graph.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the graph's minimum
#' # eccentricity
#' graph %>%
#'   get_min_eccentricity()
#'
#' # Create a full graph and then
#' # get the minimum eccentricity
#' # value for that
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   get_min_eccentricity()
#'
#' @export
get_min_eccentricity <- function(
    graph,
    direction = "all"
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  if (!(direction %in% c("all", "in", "out"))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "Valid options for `direction` are `all`, `in`, or `out`.")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the minimum eccentricity of the graph
  igraph::radius(ig_graph, mode = direction)
}
