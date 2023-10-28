#' Get graph adhesion
#'
#' @description
#'
#' Get the adhesion of a graph, which is the minimum number of edges needed to
#' remove to obtain a graph which is not strongly connected. This is the same as
#' the edge connectivity of the graph.
#'
#' @inheritParams render_graph
#'
#' @return A single numeric value representing the minimum number of edges to
#'   remove.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the graph's adhesion
#' graph %>% get_adhesion()
#'
#' # Create a full graph and then
#' # get the adhesion for that
#' create_graph() %>%
#'   add_full_graph(n = 8) %>%
#'   get_adhesion()
#'
#' @export
get_adhesion <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(NA_real_)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the number of edges required for
  # removal to transition to state where
  # the graph is no longer strongly connected
  igraph::edge_connectivity(ig_graph)
}
