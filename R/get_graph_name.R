#' Get graph name
#' @description Get the name of a graph object of class
#' \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single-length character vector with the
#' assigned graph name. If a graph name has not been
#' set, NA is returned.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a name
#' graph <-
#'   set_graph_name(
#'     graph,
#'     name = "the_name")
#'
#' # Get the graph's name
#' get_graph_name(graph)
#' @export get_graph_name

get_graph_name <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  graph$graph_info$graph_name
}
