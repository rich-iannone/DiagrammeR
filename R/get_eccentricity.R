#' Get node eccentricities
#' @description Get a data frame with node
#' eccentricity values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame containing eccentricity values
#' by node ID value.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the eccentricity values for all
#' # nodes in the graph
#' graph %>%
#'   get_eccentricity()
#' #>    id eccentricity
#' #> 1   1            4
#' #> 2   2            4
#' #> 3   3            3
#' #> 4   4            4
#' #> 5   5            3
#' #> 6   6            2
#' #> 7   7            2
#' #> 8   8            0
#' #> 9   9            1
#' #> 10 10            0
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
  data.frame(
    id = eccentricity %>%
      names() %>%
      as.integer(),
    eccentricity = eccentricity,
    stringsAsFactors = FALSE)
}
