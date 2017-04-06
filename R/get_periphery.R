#' Get nodes that form the graph periphery
#' @description Get those nodes that are part of the
#' graph periphery (i.e., have the maximum eccentricity
#' in the graph).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector of node IDs.
#' @examples
#' # Get the nodes that are in the graph periphery
#' # of a randomly-created graph
#' get_periphery(
#'   create_random_graph(
#'     15, 24,
#'     set_seed = 23))
#' #> [1] 2
#' @importFrom dplyr filter select
#' @importFrom purrr flatten_chr
#' @export get_periphery

get_periphery <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  id <- NULL

  # Get the eccentricity for each of the graph's nodes
  eccentricity <- get_eccentricity(graph)

  # Get the node ID values for all nodes where the
  # eccentricity is equal to the graph diameter
  # (i.e., maximum eccentricity)
  periphery_nodes <-
    eccentricity %>%
    dplyr::filter(eccentricity == get_graph_diameter(graph)) %>%
    dplyr::select(id) %>%
    purrr::flatten_chr() %>%
    as.integer()

  return(periphery_nodes)
}
