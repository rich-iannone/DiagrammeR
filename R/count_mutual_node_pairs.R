#' Get the number of mutually-connected node pairs
#' @description Get the number of
#' mutually-connected node pairs.
#' This works for directed graphs.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single numeric value
#' representing the number of mutually-
#' connected node pairs.
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of mutually-connected
#' # node pairs
#' graph %>%
#'   count_mutual_node_pairs()
#' #> [1] 0
#'
#' # Create a full graph and then
#' # count the mutually-connected
#' # node pairs
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   count_mutual_node_pairs()
#' #> [1] 45
#' @importFrom igraph dyad_census
#' @export count_mutual_node_pairs

count_mutual_node_pairs <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the number of mutually-connected node pairs
  # in the graph
  unname(unlist(igraph::dyad_census(ig_graph)["mut"]))
}
