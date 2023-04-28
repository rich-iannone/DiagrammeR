#' Add clones of a selection of nodes
#'
#' @description
#'
#' Add new nodes to a graph object of class `dgr_graph` which are clones of
#' nodes in an active selection of nodes. All node attributes are preserved
#' except for the node `label` attribute (to maintain the uniqueness of non-`NA`
#' node label values). A vector of node `label` can be provided to bind new
#' labels to the cloned nodes.
#'
#' This function makes use of an active selection of nodes (and the function
#' ending with `_ws` hints at this).
#'
#' Selections of nodes can be performed using the following node selection
#' (`select_*()`) functions: [select_nodes()], [select_last_nodes_created()],
#' [select_nodes_by_degree()], [select_nodes_by_id()], or
#' [select_nodes_in_neighborhood()].
#'
#' Selections of nodes can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out()], [trav_in()], [trav_both()],
#' [trav_out_node()], [trav_in_node()], [trav_out_until()], or
#' [trav_in_until()].
#'
#' @inheritParams render_graph
#' @param add_edges An option for whether to add edges from the selected nodes
#'   to each of their clones, or, in the opposite direction.
#' @param direction Using `from` will create new edges from existing nodes to
#'   the new, cloned nodes. The `to` option will create new edges directed
#'   toward the existing nodes.
#' @param label An optional vector of node label values. The vector length
#'   should correspond to the number of nodes in the active selection of nodes.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with a path of
#' # nodes; supply `label`, `type`,
#' # and `value` node attributes,
#' # and select the created nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     label = c("d", "g", "r"),
#'     type = c("a", "b", "c")) %>%
#'   select_last_nodes_created()
#'
#' # Display the graph's internal
#' # node data frame
#' graph %>% get_node_df()
#'
#' # Create clones of all nodes
#' # in the selection but assign
#' # new node label values
#' # (leaving `label` as NULL
#' # yields NA values)
#' graph <-
#'   graph %>%
#'   add_node_clones_ws(
#'     label = c("a", "b", "v"))
#'
#' # Display the graph's internal
#' # node data frame: nodes `4`,
#' # `5`, and `6` are clones of
#' # `1`, `2`, and `3`
#' graph %>% get_node_df()
#'
#' # Select the last nodes
#' # created (`4`, `5`, and `6`)
#' # and clone those nodes and
#' # their attributes while
#' # creating new edges between
#' # the new and existing nodes
#' graph <-
#'   graph %>%
#'   select_last_nodes_created() %>%
#'   add_node_clones_ws(
#'     add_edges = TRUE,
#'     direction = "to",
#'     label = c("t", "z", "s"))
#'
#' # Display the graph's internal
#' # edge data frame; there are
#' # edges between the selected
#' # nodes and their clones
#' graph %>% get_edge_df()
#'
#' @family Node creation and removal
#'
#' @export
add_node_clones_ws <- function(
    graph,
    add_edges = FALSE,
    direction = NULL,
    label = NULL
) {

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

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes, so, clones of nodes cannot be added")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of nodes available.")
  }

  # # Stop function if vector provided for label but it
  # # is not of length `n`
  # if (!is.null(label)) {
  #   if (length(label) != n) {
  #     stop(
  #       "The vector provided for `label` is not the same length as the value of `n`."),
  #       call. = FALSE
  #   }
  # }

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    graph$graph_log$version_id %>%
    max()

  # Get the number of columns in the graph's
  # internal node data frame
  n_col_ndf <-
    graph %>%
    get_node_df() %>%
    ncol()

  # Get the node ID values for
  # the nodes in the active selection
  selected_nodes <- suppressMessages(get_selection(graph))

  # Clear the graph's selection
  graph <-
    suppressMessages(
      graph %>%
        clear_selection())

  # Get the number of nodes in the graph
  nodes_graph_1 <-
    graph %>%
    count_nodes()

  # Get the number of edges in the graph
  edges_graph_1 <-
    graph %>%
    count_edges()

  node_id_value <- graph$last_node

  for (i in 1:length(selected_nodes)) {

    # Extract all of the node attributes
    # (`type` and additional node attrs)
    node_attr_vals <-
      graph %>%
      get_node_df() %>%
      dplyr::filter(id %in% selected_nodes[i]) %>%
      dplyr::select(-id, -label)

    # Create a clone of the selected
    # node in the graph
    graph <-
      graph %>%
      add_node(
        label = label[i])

    # Obtain the node ID value for
    # the new node
    new_node_id <-
      graph$nodes_df[nrow(graph$nodes_df), 1]

    # Create a node selection for the
    # new nodes in the graph
    graph <-
      graph %>%
      select_nodes_by_id(
        nodes = new_node_id)

    # Iteratively set node attribute values for
    # the new nodes in the graph
    for (j in 1:ncol(node_attr_vals)) {
      for (k in 1:length(new_node_id)) {

        graph$nodes_df[
          which(graph$nodes_df[, 1] == new_node_id[k]),
          which(colnames(graph$nodes_df) == colnames(node_attr_vals)[j])] <-
          node_attr_vals[[j]]
      }
    }

    # Create an edge if `add_edges = TRUE`
    if (add_edges) {

      if (direction == "from") {
        graph <-
          graph %>%
          add_edge(
            from = new_node_id,
            to = selected_nodes[i])
      } else {
        graph <-
          graph %>%
          add_edge(
            from = selected_nodes[i],
            to = new_node_id)
      }
    }

    # Increment the node ID value
    node_id_value <- node_id_value + 1

    # Clear the graph's active selection
    graph <-
      suppressMessages(
        graph %>%
          clear_selection())
  }

  # Remove extra items from the `graph_log`
  graph$graph_log <-
    graph$graph_log %>%
    dplyr::filter(version_id <= current_graph_log_version_id)

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- graph %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_added <- nodes_graph_2 - nodes_graph_1

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

  # Update the `last_node` value
  graph$last_node <- max(graph$nodes_df$id)

  # Update the `last_edge` value
  graph$last_edge <- max(graph$edges_df$id)

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
      d_n = nodes_added,
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
