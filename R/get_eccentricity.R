#' Get node eccentricities
#' @description Get a named vector or data frame with
#' node eccentricity values
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes an optional vector of node IDs for
#' filtering the list of nodes present in the graph.
#' @return a data frame containing metrics pertaining
#' to the graph
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
      get_paths(
        graph,
        from = get_nodes(graph)[i],
        longest_path = TRUE) %>%
      unlist %>% length

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
