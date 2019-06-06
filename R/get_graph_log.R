#' Get the graph log information
#'
#' Get a tibble of the graph log, which contains information on the functions
#' called on the graph that resulted in some transformation of the graph.
#'
#' @inheritParams render_graph
#' @return A `df_tbl` object.
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
#' graph %>% get_graph_log()
#'
#' @export
get_graph_log <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  graph$graph_log %>%
    dplyr::as_tibble()
}
