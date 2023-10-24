#' Get the number of automorphisms
#'
#' @description
#'
#' Get the number of automorphisms the graph contains. An automorphism of a
#' graph is a form of symmetry in which the graph is mapped onto itself while
#' preserving edge-node connectivity.
#'
#' @inheritParams render_graph
#'
#' @return A single numeric value representing the number of automorphisms the
#'   graph contains.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of automorphisms
#' graph %>%
#'   count_automorphisms()
#'
#' # Create a full graph and then
#' # count the automorphisms
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   count_automorphisms()
#'
#' @export
count_automorphisms <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(NA_real_)
  }

  # Convert the graph to an undirected graph
  graph_undirected <- set_graph_undirected(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph_undirected)

  # Get the number of automorphisms in
  # the graph
  res <- unlist(igraph::automorphisms(
    graph = ig_graph,
    sh = "fm")["group_size"])

  as.numeric(res)
}
