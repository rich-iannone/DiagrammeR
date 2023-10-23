#' Traverse to any reverse edges
#'
#' @description
#'
#' From an active selection of edges in a graph object of class `dgr_graph`,
#' traverse to any available reverse edges between the nodes common to the
#' selected edges. For instance, if an active selection has the edge `1->2` but
#' there is also an (not selected) edge `2->1`, then this function can either
#' switch to the selection of `2->1`, or, incorporate both those edges into the
#' active selection of edges.
#'
#' This traversal function makes use of an active selection of edges. After the
#' traversal, depending on the traversal conditions, there will either be a
#' selection of edges or no selection at all.
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
#' @param add_to_selection An option to either add the reverse edges to the
#'   active selection of edges (`TRUE`) or switch the active selection entirely
#'   to those reverse edges (`FALSE`, the default case).
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE)
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 4, 2, 3, 3),
#'     to =   c(4, 1, 3, 2, 1))
#'
#' # Create a graph with the
#' # ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select the edges
#' # `1`->`4` and `2`->`3`
#' graph <-
#'   graph %>%
#'   select_edges(
#'     from = 1,
#'       to = 4) %>%
#'   select_edges(
#'     from = 2,
#'       to = 3)
#'
#' # Get the inital edge selection
#' graph %>% get_selection()
#'
#' # Traverse to the reverse edges
#' # (edges `2`: `4`->`1` and
#' # `4`:`3`->`2`)
#' graph <-
#'   graph %>%
#'   trav_reverse_edge()
#'
#' # Get the current selection of edges
#' graph %>% get_selection()
#'
#' @export
trav_reverse_edge <- function(
    graph,
    add_to_selection = FALSE
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Graph object has valid edge selection
  if (!graph_contains_edge_selection(graph)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no selection of edges.")
  }

  # Extract the selection of edges
  edges_from <- graph$edge_selection$from
  edges_to <- graph$edge_selection$to

  # Extract the graph's internal edf
  edf <- graph$edges_df

  # Get the available reverse edges
  reverse_edges_df <- data.frame(to = edges_from, from = edges_to)
  reverse_edges <- edf %>% dplyr::inner_join(reverse_edges_df, by = c("from", "to"))

  # Add the reverse edges to the existing,
  # selected edges
  if (add_to_selection) {
    edges_df <- data.frame(to = edges_to, from = edges_from)
    edges <-
      edf %>%
      dplyr::inner_join(edges_df, by = c("from", "to")) %>%
      dplyr::bind_rows(reverse_edges)
  } else {
    edges <- reverse_edges
  }

  # Modify `edges` to create a correct esdf
  edges <-
    edges %>%
    dplyr::select(edge = "id", "from", "to") %>%
    dplyr::arrange(edge)

  # Add the edge ID values to the active selection
  # of edges in `graph$edge_selection`
  graph$edge_selection <- edges

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

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
