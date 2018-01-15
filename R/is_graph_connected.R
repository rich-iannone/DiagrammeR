#' Is the graph a connected graph?
#' @description Determines whether a graph is a
#' connected graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
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
#' @export is_graph_connected

is_graph_connected <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  wc_components <- get_w_connected_cmpts(graph)

  if (length(unique(wc_components$wc_component)) > 1) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
