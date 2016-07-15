#' Get node eccentricities
#' @description Get a named vector or data frame with
#' node eccentricity values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes an optional vector of node IDs for
#' filtering the list of nodes present in the graph.
#' @param return_type using \code{vector} (the default)
#' will provide a named vector of eccentricity values
#' (the node IDs serve as names). With \code{df},
#' a data frame containing node IDs and eccentricity
#' values is provided.
#' @return a data frame containing metrics pertaining
#' to the graph.
#' @examples
#' # Get the eccentricities for all nodes in
#' # a randomly-created graph
#' get_eccentricity(
#'   graph = create_random_graph(
#'             15, 20, set_seed = 20))
#' #>  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
#' #>  5  8  2  7  1  1  7  7  1  6  2  2  8  7  9
#' @export get_eccentricity

get_eccentricity <- function(graph,
                             nodes = NULL,
                             return_type = "vector") {

  if (is.null(nodes)) {
    nodes_to_process <- get_nodes(graph)
    node_ids <- nodes_to_process
  }

  if (!is.null(nodes)) {

    # Stop function if nodes provided are not all
    # in the graph
    if (any(!(as.character(nodes) %in% get_nodes(graph)))) {
      stop('Not all nodes specified are present in the graph.')
    }

    nodes_to_process <- nodes
    node_ids <- nodes_to_process
  }

  for (i in 1:length(nodes_to_process)) {

    if (i == 1) {
      eccentricity <- vector(mode = 'numeric')
    }

    longest_path <-
      max(
        lengths(
          get_paths(
            graph,
            from = get_nodes(graph)[i],
            longest_path = TRUE))) - 1

    eccentricity <-
      c(eccentricity, longest_path)

    if (i == length(get_nodes(graph))) {
      names(eccentricity) <- node_ids
    }
  }

  if (return_type == 'vector') {
    return(eccentricity)
  }

  if (return_type == 'df') {

    # Create a data frame with node ID values
    # and eccentrity values
    eccentricity <-
      data.frame(node = names(lengths),
                 eccentricity = lengths,
                 stringsAsFactors = FALSE)

    return(eccentricity)
  }
}
