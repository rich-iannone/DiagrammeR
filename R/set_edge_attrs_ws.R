#' Set edge attributes with an edge selection
#'
#' @description
#'
#' From a graph object of class `dgr_graph` or an edge data frame, set edge
#' attribute properties for one or more edges.
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
#' @param edge_attr The name of the attribute to set.
#' @param value The value to be set for the chosen attribute for the edges in
#'   the current selection.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 6)
#'
#' # Select specific edges from
#' # the graph and apply the edge
#' # attribute `color = blue` to
#' # those selected edges
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(nodes = 2:4) %>%
#'   trav_out_edge() %>%
#'   set_edge_attrs_ws(
#'     edge_attr = color,
#'     value = "blue")
#'
#' # Show the internal edge data
#' # frame to verify that the
#' # edge attribute has been set
#' # for specific edges
#' graph %>% get_edge_df()
#'
#' @family edge creation and removal
#'
#' @export
set_edge_attrs_ws <- function(
    graph,
    edge_attr,
    value
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Graph object has valid edge selection
  check_graph_contains_edge_selection(graph)

  # Get the requested `edge_attr`
  edge_attr <-
    rlang::enquo(edge_attr) %>% rlang::get_expr() %>% as.character()

  # Get vectors of edge ID values for the
  # edge selection
  edge_ids <- graph$edge_selection$edge

  # Update the graph's internal edf
  if (edge_attr %in% colnames(graph$edges_df)) {

    graph$edges_df[
      which(graph$edges_df[, 1] %in% edge_ids),
      which(colnames(graph$edges_df) %in% edge_attr)] <- value

  } else {
    graph$edges_df <-
      graph$edges_df %>%
      dplyr::mutate(edge_attr__ = dplyr::case_when(
        id %in% edge_ids ~ value))

    colnames(graph$edges_df)[length(colnames(graph$edges_df))] <-
      edge_attr
  }

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
