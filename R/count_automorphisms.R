#' Get the number of automorphisms in the graph
#' @description Get the number of automorphisms the
#' graph contains. An automorphism of a graph is a
#' form of symmetry in which the graph is mapped onto
#' itself while preserving edge-node connectivity.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single numeric value representing the
#' number of automorphisms the graph contains.
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of automorphisms
#' count_automorphisms(graph)
#' #> [1] 10
#'
#' # Create a full graph and then
#' # count the automorphisms
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   count_automorphisms()
#' #> [1] 3628800
#' @importFrom igraph automorphisms
#' @export count_automorphisms

count_automorphisms <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an undirected graph
  graph_undirected <- set_graph_undirected(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph_undirected)

  # Get the number of automorphisms in
  # the graph
  automorphisms(
    graph = ig_graph,
    sh = "fm")["group_size"] %>%
    unlist() %>%
    as.numeric()
}
