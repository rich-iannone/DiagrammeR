#' Get graph girth
#' @description Get the girth of a graph, which is
#' the length of the shortest circle in the graph.
#' Loop edges and multiple edges are not considered.
#' If the graph contains no cycles then zero is
#' returned.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single numeric value representing the
#' length of the shortest circle in the graph.
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the graph's girth
#' get_girth(graph)
#' #> [1] 5
#'
#' # Create a full graph and then
#' # get the girth for that
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   get_girth()
#' #> [1] 3
#' @importFrom igraph girth
#' @export get_girth

get_girth <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the girth of the graph
  igraph::girth(ig_graph, circle = FALSE)$girth
}
