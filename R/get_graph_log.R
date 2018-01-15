#' Get the graph log information
#' @description Get a tibble of the graph log, which
#' contains information on the functions called on
#' the graph that resulted in some transformation of
#' the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a \code{df_tbl} object.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function and
#' # delete 2 nodes from the graph
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23) %>%
#'   delete_node(node = 5) %>%
#'   delete_node(node = 7)
#'
#' # Get the graph log, which is a
#' # record of all graph transformations
#' graph %>%
#'   get_graph_log()
#' @importFrom dplyr as_tibble
#' @export get_graph_log

get_graph_log <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  graph$graph_log %>%
    dplyr::as_tibble()
}
