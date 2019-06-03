#' Add new edges in the opposite directions of a selection of edges
#'
#' Add edges in the opposite direction of one or more edges available as an edge
#' selection in a graph object of class \code{dgr_graph}. New graph edges have
#' the opposite edge definitions as those in the selection. For example, a graph
#' with the edge \code{1->2} in its active selection will gain a new \code{2->1}
#' edge. There is also the option to assign a common \code{rel} grouping to the
#' newly created edges. Upon addition of the edges, the edge selection will be
#' retained for further selection or traversal operations.
#'
#' This function makes use of an active selection of edges (and the function
#' ending with \code{_ws} hints at this).
#'
#' Selections of edges can be performed using the following selection
#' (\code{select_*()}) functions:
#' \code{\link{select_edges}()},
#' \code{\link{select_last_edges_created}()},
#' \code{\link{select_edges_by_edge_id}()}, or
#' \code{\link{select_edges_by_node_id}()}.
#'
#' Selections of edges can also be performed using the following traversal
#' (\code{trav_*()}) functions:
#' \code{\link{trav_out_edge}()},
#' \code{\link{trav_in_edge}()},
#' \code{\link{trav_both_edge}()}, or
#' \code{\link{trav_reverse_edge}()}.
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param rel an optional string to apply a \code{rel} attribute to all newly
#'   created edges.
#' @return a graph object of class \code{dgr_graph}.
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
#' @importFrom dplyr select bind_rows as_tibble
#' @export
add_reverse_edges_ws <- function(graph,
                                 rel = NULL,
                                 edge_aes = NULL,
                                 edge_data = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no selection of edges")
  }

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # If no value(s) provided for `rel`, set to NA
  if (is.null(rel)) {
    rel <- as.character(NA)
  }

  # Get a vector of edges available in the
  # graph's selection
  edges_in_selection <-
    graph$edge_selection %>%
    dplyr::select(to, from)

  # Add new edges to the graph for every edge
  # in the graph's active selection
  for (i in 1:nrow(edges_in_selection)) {

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

      edge_aes$index__ <- 1:edges_added

      edge_aes_tbl <-
        dplyr::as_tibble(edge_aes) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(edge_aes_tbl)) {
      edge_aes_tbl <-
        edge_aes_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- dplyr::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < edges_added) {

      edge_data$index__ <- 1:edges_added

      edge_data_tbl <-
        dplyr::as_tibble(edge_data) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(edge_data_tbl)) {
      edge_data_tbl <-
        edge_data_tbl %>%
        dplyr::select(-id)
    }
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    graph$edges_df <-
      bind_rows(
        graph$edges_df[1:(nrow(graph$edges_df) - edges_added), ],
        bind_cols(
          graph$edges_df[(nrow(graph$edges_df) - edges_added + 1):nrow(graph$edges_df), ],
          edge_aes_tbl))
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    graph$edges_df <-
      bind_rows(
        graph$edges_df[1:(nrow(graph$edges_df) - edges_added), ],
        bind_cols(
          graph$edges_df[(nrow(graph$edges_df) - edges_added + 1):nrow(graph$edges_df), ],
          edge_data_tbl))
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_e = edges_added)

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
