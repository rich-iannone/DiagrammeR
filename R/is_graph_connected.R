#' Is the graph a connected graph?
#'
#' Determines whether a graph is a connected graph.
#'
#' @inheritParams render_graph
#'
#' @return A logical value.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function; this
#' # graph is not connected
#' create_graph() %>%
#'   add_gnm_graph(
#'     n = 15,
#'     m = 10,
#'     set_seed = 23) %>%
#'   is_graph_connected()
#'
#' # Create another random graph;
#' # this graph is connected
#' create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23) %>%
#'   is_graph_connected()
#'
#' @export
is_graph_connected <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  wc_components <- get_w_connected_cmpts(graph)

  if (length(unique(wc_components$wc_component)) > 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
