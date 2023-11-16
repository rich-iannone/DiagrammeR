#' Get the graph girth
#'
#' @description
#'
#' Get the girth of a graph, which is the length of the shortest circle in the
#' graph. Loop edges and multiple edges are not considered. If the graph
#' contains no cycles then zero is returned.
#'
#' @inheritParams render_graph
#'
#' @return A single numeric value representing the length of the shortest circle
#'   in the graph.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the graph's girth
#' graph %>% get_girth()
#'
#' # Create a full graph and then
#' # get the girth for that
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   get_girth()
#'
#' @export
get_girth <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(NA_real_)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the girth of the graph
  igraph::girth(ig_graph, circle = FALSE)$girth
}
