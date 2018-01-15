#' Get global graph attributes
#' @description Get the presently set global attributes
#' for a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame containing global attributes
#' for the graph.
#' @examples
#' # Create a new graph and set some global attributes
#' graph <-
#'   create_graph() %>%
#'   set_global_graph_attrs(
#'     attr = "overlap",
#'     value = "true",
#'     attr_type = "graph")
#'
#' # View the graph's set of global attributes
#' # as a data frame
#' get_global_graph_attrs(graph)
#' @export get_global_graph_attrs

get_global_graph_attrs <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  if (nrow(graph$global_attrs) == 0) {
    return(NA)
  } else if (nrow(graph$global_attrs) > 0) {
    return(graph$global_attrs)
  }
}
