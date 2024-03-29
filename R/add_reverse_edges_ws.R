#' Add new edges in the opposite directions of a selection of edges
#'
#' @description
#'
#' Add edges in the opposite direction of one or more edges available as an edge
#' selection in a graph object of class `dgr_graph`. New graph edges have the
#' opposite edge definitions as those in the selection. For example, a graph
#' with the edge `1->2` in its active selection will gain a new `2->1` edge.
#' There is also the option to assign a common `rel` grouping to the newly
#' created edges. Upon addition of the edges, the edge selection will be
#' retained for further selection or traversal operations.
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
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param rel An optional string to apply a `rel` attribute to all newly created
#'   edges.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create an empty graph, add 2 nodes to it,
#' # and create the edge `1->2`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(
#'     n = 2,
#'     type = "type_a",
#'     label = c("a_1", "a_2")) %>%
#'   add_edge(
#'     from = 1,
#'     to = 2,
#'     rel = "a")
#'
#' # Get the graph's edges
#' graph %>% get_edge_ids()
#'
#' # Select the edge and create 2 additional edges
#' # with the opposite definition of `1->2`, which
#' # is `2->1`; also, apply, different `rel` values
#' # (`b` and `c`)
#' graph <-
#'   graph %>%
#'   select_edges() %>%
#'   add_reverse_edges_ws(rel = "b") %>%
#'   add_reverse_edges_ws(rel = "c") %>%
#'   clear_selection()
#'
#' # Get the graph's edge data frame
#' graph %>% get_edge_df()
#'
#' @family edge creation and removal
#'
#' @export
add_reverse_edges_ws <- function(
    graph,
    rel = NULL,
    edge_aes = NULL,
    edge_data = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Graph object has valid edge selection
  check_graph_contains_edge_selection(graph)

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # If no value(s) provided for `rel`, set to NA
  rel <- rel %||% NA_character_

  # Get a vector of edges available in the
  # graph's selection
  edges_in_selection <-
    graph$edge_selection %>%
    dplyr::select("to", "from")

  # Add new edges to the graph for every edge
  # in the graph's active selection
  for (i in seq_len(nrow(edges_in_selection))) {

    # Create a graph edge
    graph <-
      add_edge(
        graph = graph,
        from = edges_in_selection[i, 1],
        to = edges_in_selection[i, 2],
        rel = rel)

    # Redact the signing of the action to the log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]
  }

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- dplyr::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < edges_added) {

      edge_aes$index__ <- seq_len(edges_added)

      edge_aes_tbl <-
        dplyr::as_tibble(edge_aes) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(edge_aes_tbl)) {
      edge_aes_tbl$id <- NULL
    }
  }

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- dplyr::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < edges_added) {

      edge_data$index__ <- seq_len(edges_added)

      edge_data_tbl <-
        dplyr::as_tibble(edge_data) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(edge_data_tbl)) {
      edge_data_tbl$id <- NULL
    }
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    graph$edges_df <-
      dplyr::bind_rows(
        graph$edges_df[seq_len(nrow(graph$edges_df) - edges_added), ],
        dplyr::bind_cols(
          graph$edges_df[(nrow(graph$edges_df) - edges_added + 1):nrow(graph$edges_df), ],
          edge_aes_tbl))
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    graph$edges_df <-
      dplyr::bind_rows(
        graph$edges_df[1:(nrow(graph$edges_df) - edges_added), ],
        dplyr::bind_cols(
          graph$edges_df[(nrow(graph$edges_df) - edges_added + 1):nrow(graph$edges_df), ],
          edge_data_tbl))
  }

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_e = edges_added)

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      trigger_graph_actions(graph)
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
