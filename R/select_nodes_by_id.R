#' Select nodes in a graph by their ID values
#'
#' Select nodes in a graph object of class \code{dgr_graph} by their node ID
#'   values. If nodes have IDs that are monotonically increasing integer
#'   values, then numeric ranges can be used for the selection.
#' @inheritParams render_graph
#' @param nodes a vector of node IDs for the selection of nodes present in the
#'   graph.
#' @param set_op the set operation to perform upon consecutive selections of
#'   graph nodes. This can either be as a \code{union} (the default), as an
#'   intersection of selections with \code{intersect}, or, as a
#'   \code{difference} on the previous selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <- create_node_df(n = 10)
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf)
#'
#' # Select nodes `1` to `5` and show that
#' # selection of nodes with `get_selection()`
#' graph %>%
#'   select_nodes_by_id(nodes = 1:5) %>%
#'   get_selection()
#' @export
select_nodes_by_id <- function(graph,
                               nodes,
                               set_op = "union") {

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
      reasons = "The graph contains no nodes")
  }

  # Get a vector of node ID values from the graph
  nodes_in_graph <- graph$nodes_df[, 1]

  # Stop function if any nodes specified are not
  # in the graph
  if (any(!(nodes %in% nodes_in_graph))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "One of more of the nodes specified are not available in the graph")
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # Incorporate selected nodes into graph's
  # selection
  if (set_op == "union") {
    nodes_combined <-
      union(nodes_prev_selection, nodes)
  } else if (set_op == "intersect") {
    nodes_combined <-
      intersect(nodes_prev_selection, nodes)
  } else if (set_op == "difference") {
    nodes_combined <-
      base::setdiff(nodes_prev_selection, nodes)
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

  # Construct message body
  if (!n_e_select_properties_in[["node_selection_available"]] &
      !n_e_select_properties_in[["edge_selection_available"]]) {

    msg_body <-
      glue::glue(
        "created a new selection of \\
        {n_e_select_properties_out[['selection_count_str']]}")

  } else if (n_e_select_properties_in[["node_selection_available"]] |
             n_e_select_properties_in[["edge_selection_available"]]) {

    if (n_e_select_properties_in[["node_selection_available"]]) {
      msg_body <-
        glue::glue(
          "modified an existing selection of\\
           {n_e_select_properties_in[['selection_count_str']]}:
           * {n_e_select_properties_out[['selection_count_str']]}\\
           are now in the active selection
           * used the `{set_op}` set operation")
    }

    if (n_e_select_properties_in[["edge_selection_available"]]) {
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

  graph
}
