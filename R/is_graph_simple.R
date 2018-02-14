#' Is the graph a simple graph?
#' @description Determine whether the graph is
#' a simple graph. A simple graph is one that does
#' not contain any loops nor any multiple edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' # Create a graph with 2 cycles
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 4) %>%
#'   add_cycle(n = 3)
#'
#' # Check if the graph is simple
#' graph %>%
#'   is_graph_simple()
#' @importFrom igraph is_simple
#' @export is_graph_simple

is_graph_simple <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Determine whether the graph is
  # a simple graph
  igraph::is_simple(ig_graph)
}
