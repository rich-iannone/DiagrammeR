#' Reverse the direction of selected edges in a graph using an edge selection
#'
#' @description
#'
#' Using a directed graph with a selection of edges as input, reverse the
#' direction of those selected edges in input graph.
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
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with a
#' # directed tree
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 2)
#'
#' # Inspect the graph's edges
#' graph %>% get_edges()
#'
#' # Select all edges associated
#' # with nodes `1` and `2`
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(
#'     nodes = 1:2)
#'
#' # Reverse the edge directions
#' # of the edges associated with
#' # nodes `1` and `2`
#' graph <-
#'   graph %>%
#'   rev_edge_dir_ws()
#'
#' # Inspect the graph's edges
#' # after their reversal
#' graph %>% get_edges()
#'
#' @family edge creation and removal
#'
#' @export
rev_edge_dir_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Graph object has valid edge selection
  check_graph_contains_edge_selection(graph)

  # If graph is undirected, stop function
  if (!graph$directed) {
    cli::cli_abort("The input graph must be a directed graph.")
  }

  # Get the graph nodes in the `from` and `to` columns
  # of the edf
  from <- get_edges(graph, return_type = "df")[, 1]
  to <- get_edges(graph, return_type = "df")[, 2]

  # Extract the graph's edge data frame
  edges <- get_edge_df(graph)

  # Get edge ID values in edge selection
  edge_ids <- suppressMessages(get_selection(graph))

  # Selectively modify the edge direction and create
  # a new edf
  edges_new <-
    edges %>%
    dplyr::filter(id %in% edge_ids) %>%
    dplyr::filter(from != to) %>%
    dplyr::rename(from = to, to = from) %>%
    dplyr::relocate("id", "from", "to") %>%
    dplyr::bind_rows(edges %>% dplyr::filter(!(id %in% edge_ids)))

  # Modify the graph object
  graph$edges_df <- edges_new

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
