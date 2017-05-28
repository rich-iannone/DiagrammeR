#' Get the graph diameter
#' @description Get the graph diameter, which is the
#' maximum eccentricity.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return the graph diameter as a single-length
#' vector.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the graph diameter
#' get_graph_diameter(graph)
#' #> [1] 4
#' @importFrom dplyr group_by summarize ungroup
#' @importFrom purrr flatten_dbl
#' @export get_graph_diameter

get_graph_diameter <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  eccentricity <- NULL

  # Get the graph diameter by obtaining the
  # maximum eccentricity for all nodes in
  # the graph
  graph_diameter <-
    graph %>%
    get_eccentricity() %>%
    dplyr::group_by() %>%
    dplyr::summarize(max_eccentricity = max(eccentricity)) %>%
    dplyr::ungroup() %>%
    purrr::flatten_dbl() %>%
    as.integer()

  return(graph_diameter)
}
