#' Add one or several clones of an existing node to the graph
#'
#' @description
#'
#' Add `n` new nodes to a graph object of class `dgr_graph` which are clones of
#' a node already in the graph. All node attributes are preserved except for the
#' node `label` attribute (to maintain the uniqueness of non-`NA` node label
#' values). A vector of node `label` can be provided to bind new labels to the
#' cloned nodes.
#'
#' @inheritParams render_graph
#' @param n The number of node clones to add to the graph.
#' @param node A node ID corresponding to the graph node to be cloned.
#' @param label An optional vector of node label values. The vector length
#'   should correspond to the value set for `n`.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with a path of
#' # nodes; supply `label`, `type`,
#' # and `value` node attributes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     label = c("d", "g", "r"),
#'     type = c("a", "b", "c"))
#'
#' # Display the graph's internal
#' # node data frame
#' graph %>% get_node_df()
#'
#' # Create 3 clones of node `1`
#' # but assign new node label
#' # values (leaving `label` as
#' # NULL yields NA values)
#' graph <-
#'   graph %>%
#'   add_n_node_clones(
#'     n = 3,
#'     node = 1,
#'     label = c("x", "y", "z"))
#'
#' # Display the graph's internal
#' # node data frame: nodes `4`,
#' # `5`, and `6` are clones of `1`
#' graph %>% get_node_df()
#'
#' @family node creation and removal
#'
#' @export
add_n_node_clones <- function(
    graph,
    n,
    node,
    label = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph, extra_msg = "So, clones of nodes cannot be added.")

  # Stop function if node is not a single numerical value
  check_number_decimal(node)

  # Stop function the node ID does not correspond
  # to a node in the graph
  if (!(node %in% graph$nodes_df$id)) {

    cli::cli_abort(c(
      "The value of `node` ({node}) must correspond to a node in the graph.",
      "Any of {.or {unique(graph$nodes_df$id)}} is acceptable.")
      )
  }

  # Stop function if vector provided for label but it
  # is not of length `n`
  if (!is.null(label)) {

    # a check_length should exist soon in rlang
    # https://github.com/r-lib/rlang/issues/1618
    check_number_whole(length(label), min = n, max = n)
  }

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    max(graph$graph_log$version_id)

  # Get the number of columns in the graph's
  # internal node data frame
  n_col_ndf <-
    graph %>%
    get_node_df() %>%
    ncol()

  # Extract all of the node attributes
  # (`type` and additional node attrs)
  if (n_col_ndf >= 4) {

    node_attr_vals <-
      graph %>%
      get_node_df() %>%
      dplyr::filter(id == node) %>%
      dplyr::select("type", 4:dplyr::all_of(n_col_ndf))
  }

  # Create one or more clones of
  # the selected node in the graph
  graph_type <- as.character(
    get_node_attrs(graph = graph, node_attr = type, nodes = node))

  graph <-
    graph %>%
    add_n_nodes(
      n = n,
      type = graph_type,
      label = label)

  # Obtain the node ID values for
  # the new nodes
  new_node_ids <-
    suppressMessages(
      graph %>%
        select_last_nodes_created() %>%
        get_selection())

  # Create a node selection for the
  # new nodes in the graph
  graph <-
    graph %>%
    select_nodes_by_id(
      nodes = new_node_ids)

  # Iteratively set node attribute values for
  # the new nodes in the graph

  if (exists("node_attr_vals")) {

    for (i in seq_len(ncol(node_attr_vals))) {
      for (j in seq_along(new_node_ids)) {

        graph$nodes_df[
          which(graph$nodes_df[, 1] == new_node_ids[j]),
          which(colnames(graph$nodes_df) == colnames(node_attr_vals)[i])] <-
          node_attr_vals[[i]]
      }
    }
  }

  # Clear the graph's active selection
  graph <-
    suppressMessages(
      clear_selection(graph))

  # Remove extra items from the `graph_log`
  graph$graph_log <-
    graph$graph_log %>%
    dplyr::filter(version_id <= current_graph_log_version_id)

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
      edges = nrow(graph$edges_df),
      d_n = n)

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
