#' Get detailed information on edges
#' @description Obtain a data frame
#' with detailed information on edges
#' and their interrelationships
#' within the graph.
#' @param graph a graph object of
#' class \code{dgr_graph}.
#' @return a data frame containing
#' information specific to each edge
#' within the graph.
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
#' graph %>%
#'   get_edge_info()
#' @importFrom dplyr select
#' @export get_edge_info

get_edge_info <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  id <- from <- to <- rel <- NULL

  # For graphs with no edges, return NA
  if (nrow(graph$edges_df) == 0) {
    return(NA)
  }

  # Extract only the first 4 columns of the
  # edge data frame
  graph$edges_df %>%
    dplyr::select(id, from, to, rel)
}
