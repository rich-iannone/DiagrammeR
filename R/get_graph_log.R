#' Get the graph log information
#' @description Get a tibble of the graph log, which
#' contains information on the functions called on
#' the graph that resulted in some transformation of
#' the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a \code{df_tbl} object.
#' @importFrom tibble as_tibble
#' @export get_graph_log

get_graph_log <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  graph$graph_log %>% as_tibble()
}
