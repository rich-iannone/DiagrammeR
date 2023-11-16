#' Get the number of mutually-connected node pairs
#'
#' @description
#'
#' Get the number of mutually-connected node pairs. This works for directed
#' graphs.
#'
#' @inheritParams render_graph
#'
#' @return A single numeric value representing the number of mutually-connected
#'   node pairs.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of mutually-connected
#' # node pairs
#' graph %>% count_mutual_node_pairs()
#'
#' # Create a full graph and then
#' # count the mutually-connected
#' # node pairs
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   count_mutual_node_pairs()
#'
#' @export
count_mutual_node_pairs <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(NA_real_)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the number of mutually-connected node pairs
  # in the graph
  unname(unlist(igraph::dyad_census(ig_graph)["mut"]))
}
