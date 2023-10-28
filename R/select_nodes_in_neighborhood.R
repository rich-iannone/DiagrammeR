#' Select nodes based on a walk distance from a specified node
#'
#' @description
#'
#' Select those nodes in the neighborhood of nodes connected a specified
#' distance from an initial node.
#'
#' @inheritParams render_graph
#' @param node The node from which the traversal will originate.
#' @param distance The maximum number of steps from the `node` for inclusion in
#'   the selection.
#' @param set_op The set operation to perform upon consecutive selections of
#'   graph nodes. This can either be as a `union` (the default), as an
#'   intersection of selections with `intersect`, or, as a `difference` on the
#'   previous selection, if it exists.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph containing
#' # a balanced tree
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 2)
#'
#' # Create a graph selection by
#' # selecting nodes in the
#' # neighborhood of node `1`, where
#' # the neighborhood is limited by
#' # nodes that are 1 connection
#' # away from node `1`
#' graph <-
#'   graph %>%
#'   select_nodes_in_neighborhood(
#'     node = 1,
#'     distance = 1)
#'
#' # Get the selection of nodes
#' graph %>% get_selection()
#'
#' # Perform another selection
#' # of nodes, this time with a
#' # neighborhood spanning 2 nodes
#' # from node `1`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes_in_neighborhood(
#'     node = 1,
#'     distance = 2)
#'
#' # Get the selection of nodes
#' graph %>% get_selection()
#'
#' @export
select_nodes_in_neighborhood <- function(
    graph,
    node,
    distance,
    set_op = "union"
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # Create an empty list object
  nodes <- list()

  # Find nodes belonging to the neighborhood
  for (i in seq_len(distance)) {
    if (i == 1) {

      nodes[[i]] <- vector(mode = "integer")

      nodes[[i]] <-
        c(node,
          get_edges(
            graph,
            return_type = "df")[
              which(
                get_edges(
                  graph,
                  return_type = "df")[, 1] ==
                  node), 2],
          get_edges(
            graph,
            return_type = "df")[
              which(
                get_edges(
                  graph,
                  return_type = "df")[, 2] ==
                  node), 1])
    }

    if (i > 1) {
      for (j in seq_along(nodes[[i - 1]])) {
        if (j == 1) {
          nodes[[i]] <- vector(mode = "integer")
        }

        nodes[[i]] <-
          c(nodes[[i]],
            get_edges(
              graph,
              return_type = "df")[
                which(
                  get_edges(
                    graph,
                    return_type = "df")[, 1] ==
                    nodes[[i - 1]][j]), 2],
            get_edges(
              graph,
              return_type = "df")[
                which(
                  get_edges(
                    graph,
                    return_type = "df")[, 2] ==
                    nodes[[i - 1]][j]), 1])
      }
    }
  }

  # From list of nodes, obtain vector of unique
  # nodes as neighbors
  nodes_selected <- unique(unlist(nodes))

  # If no node ID values in `nodes_selected` return
  # the graph without a changed node selection
  if (length(nodes_selected) == 0) {
    return(graph)
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  # Incorporate selected nodes into graph's selection
  if (set_op == "union") {
    nodes_combined <-
      union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <-
      intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <-
      base::setdiff(nodes_prev_selection, nodes_selected)
  }

  # Add the node ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <-
    replace_graph_node_selection(
      graph = graph,
      replacement = nodes_combined)

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Obtain the output graph's node and edge
  # selection properties
  n_e_select_properties_out <-
    node_edge_selection_properties(graph = graph)

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

  # Emit a message about the modification of a selection
  # if that option is set
  if (!is.null(graph$graph_info$display_msgs) &&
      graph$graph_info$display_msgs) {

    # Construct message body
    if (!n_e_select_properties_in[["node_selection_available"]] &
        !n_e_select_properties_in[["edge_selection_available"]]) {

      msg_body <-
        glue::glue(
          "created a new selection of \\
        {n_e_select_properties_out[['selection_count_str']]}")

    } else if (n_e_select_properties_in[["node_selection_available"]] |
               n_e_select_properties_in[["edge_selection_available"]]) {

      if (n_e_select_properties_in[["edge_selection_available"]]) {
        msg_body <-
          glue::glue(
            "modified an existing selection of\\
           {n_e_select_properties_in[['selection_count_str']]}:
           * {n_e_select_properties_out[['selection_count_str']]}\\
           are now in the active selection
           * used the `{set_op}` set operation")
      }

      if (n_e_select_properties_in[["node_selection_available"]]) {
        msg_body <-
          glue::glue(
            "created a new selection of\\
           {n_e_select_properties_out[['selection_count_str']]}:
           * this replaces\\
           {n_e_select_properties_in[['selection_count_str']]}\\
           in the prior selection")
      }
    }

    # Issue a message to the user
    emit_message(
      fcn_name = fcn_name,
      message_body = msg_body)
  }

  graph
}
