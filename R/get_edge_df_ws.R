#' Get the graph's edf filtered by a selection of edges
#' @description From a graph object of class
#' \code{dgr_graph}, get the graph's internal
#' edge data frame that is filtered by the edge
#' ID values currently active as a selection.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return an edge data frame.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 4,
#'     m = 4,
#'     set_seed = 23) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = c(2.5, 8.2, 4.2, 2.4))
#'
#' # Select edges with ID values
#' # `1` and `3`
#' graph <-
#'   graph %>%
#'   select_edges_by_edge_id(
#'     edges = c(1, 3))
#'
#' # Get the edge data frame that's
#' # limited to the rows that correspond
#' # to the edge selection
#' graph %>%
#'   get_edge_df_ws()
#' #>   id from to  rel value
#' #> 1  1    2  1 <NA>   2.5
#' #> 2  3    3  2 <NA>   4.2
#' @importFrom dplyr filter
#' @export get_edge_df_ws

get_edge_df_ws <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has a valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges available.")
  }

  # Create binding for specific variable
  id <- NULL

  # Extract the edge data frame (edf)
  # from the graph and get only those edges
  # from the edges selection
  graph$edges_df %>%
    dplyr::filter(id %in% graph$edge_selection$edge)
}
