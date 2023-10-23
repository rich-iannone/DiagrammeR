#' Get the graph's edf filtered by a selection of edges
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, get the graph's internal edge data
#' frame that is filtered by the edge ID values currently active as a selection.
#'
#' This function makes use of an active selection of edges (and the function
#' ending with `_ws` hints at this).
#'
#' Selections of edges can be performed using the following selection
#' (`select_*()`) functions: [select_edges()], [select_last_edges_created()],
#' [select_edges_by_edge_id()], or [select_edges_by_node_id()].
#'
#' Selections of edges can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out_edge()], [trav_in_edge()],
#' [trav_both_edge()], or [trav_reverse_edge()].
#'
#' @inheritParams render_graph
#'
#' @return an edge data frame.
#'
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
#' graph %>% get_edge_df_ws()
#'
#' @export
get_edge_df_ws <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph object has a valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of edges available.")
  }

  # Extract the edge data frame (edf)
  # from the graph and get only those edges
  # from the edges selection
  graph$edges_df %>%
    dplyr::filter(id %in% graph$edge_selection$edge)
}
