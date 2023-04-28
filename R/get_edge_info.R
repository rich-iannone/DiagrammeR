#' Get detailed information on edges
#'
#' @description
#'
#' Obtain a data frame with detailed information on edges and their
#' interrelationships within the graph.
#'
#' @inheritParams render_graph
#'
#' @return A data frame containing information specific to each edge within the
#'   graph.
#'
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5, m = 10,
#'     set_seed = 23)
#'
#' # Get information on the
#' # graph's edges
#' graph %>% get_edge_info()
#'
#' @export
get_edge_info <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # For graphs with no edges, return NA
  if (nrow(graph$edges_df) == 0) {
    return(NA)
  }

  # Extract only the first 4 columns of the
  # edge data frame
  graph$edges_df %>%
    dplyr::select(id, from, to, rel)
}
