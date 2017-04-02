#' Get node eccentricities
#' @description Get a data frame with node
#' eccentricity values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame containing eccentricity values
#' by node ID value.
#' @examples
#' # Get the eccentricities for all nodes
#' # in a randomly-created graph
#' get_eccentricity(
#'   graph = create_random_graph(
#'             5, 7, set_seed = 23))
#' #>   id eccentricity
#' #> 1  1            2
#' #> 2  2            3
#' #> 3  3            2
#' #> 4  4            1
#' #> 5  5            0
#' @export get_eccentricity

get_eccentricity <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, eccentricity values cannot be determined.")
  }

  node_ids <- get_node_ids(graph)

  for (i in 1:length(node_ids)) {

    if (i == 1) {
      eccentricity <- vector(mode = 'integer')
    }

    longest_path <-
      max(
        lengths(
          get_paths(
            graph,
            from = get_node_ids(graph)[i],
            longest_path = TRUE))) - 1

    eccentricity <-
      c(eccentricity, longest_path)

    if (i == length(node_ids)) {
      names(eccentricity) <- node_ids
    }
  }

  # Create a data frame with node ID values
  # and eccentrity values
  eccentricity <-
    data.frame(
      id = names(eccentricity) %>% as.integer(),
      eccentricity = eccentricity,
      stringsAsFactors = FALSE)

  return(eccentricity)
}
