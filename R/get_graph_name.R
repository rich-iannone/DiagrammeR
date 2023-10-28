#' Get graph name
#'
#' @description
#'
#' Get the name of a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#'
#' @return A single-length character vector with the assigned graph name. If a
#'   graph name has not been set, NA is returned.
#'
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
#' graph %>% get_graph_name()
#'
#' @export
get_graph_name <- function(graph) {
  check_graph_valid(graph)
  graph$graph_info$graph_name
}
