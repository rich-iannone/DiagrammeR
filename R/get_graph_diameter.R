#' Get the graph diameter
#' @description Get the graph diameter, which is the
#' maximum eccentricity.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return the graph diameter as a single-length
#' vector.
#' @examples
#' # Get the graph diameter for a
#' # randomly-created graph
#' get_graph_diameter(
#'   create_random_graph(
#'     n = 5, m = 7,
#'     set_seed = 23))
#' #> [1] 3
#' @export get_graph_diameter

get_graph_diameter <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  return(max(get_eccentricity(graph)))
}
