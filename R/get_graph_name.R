#' Get graph name
#' @description Get the name of a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single-length character
#' vector with the assigned graph name.
#' If a graph name has not been set, NA
#' is returned.
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
#' graph %>%
#'   get_graph_name()
#' @export get_graph_name

get_graph_name <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  graph$graph_info$graph_name
}
