#' Select edges in a graph using node ID values
#'
#' @description
#'
#' Select edges in a graph object of class `dgr_graph` using node ID values. All
#' edges associated with the provided nodes will be included in the selection.
#'
#' @inheritParams render_graph
#' @param nodes A vector of node IDs for the selection of edges present in the
#'   graph.
#' @param set_op The set operation to perform upon consecutive selections of
#'   graph edges This can either be as a `union` (the default), as an
#'   intersection of selections with `intersect`, or, as a `difference` on the
#'   previous selection, if it exists.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with 5 nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Create a graph selection by selecting edges
#' # associated with nodes `1` and `2`
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(
#'     nodes = 1:2)
#'
#' # Get the selection of edges
#' graph %>% get_selection()
#'
#' # Perform another selection of edges, with nodes
#' # `1`, `2`, and `4`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges_by_node_id(
#'     nodes = c(1, 2, 4))
#'
#' # Get the selection of edges
#' graph %>% get_selection()
#'
#' # Get a fraction of the edges selected over all
#' # the edges in the graph
#' graph %>%
#'   {
#'     l <- get_selection(.) %>%
#'       length(.)
#'     e <- count_edges(.)
#'     l/e
#'   }
#'
#' @export
select_edges_by_node_id <- function(
    graph,
    nodes,
    set_op = "union"
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Extract the graph's internal edf
  edges_df <- graph$edges_df

  # Filter the edf by the requested node IDs
  edges_selected <-
    edges_df %>%
    dplyr::filter(from %in% nodes | to %in% nodes)

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # Create an integer vector representing edges
  edges_selected <- edges_selected$id

  # Obtain vector with node ID selection of edges
  # already present
  edges_prev_selection <- graph$edge_selection$edge

  # Incorporate the selected edges into the
  # graph's selection
  if (set_op == "union") {
    edges_combined <-
      union(edges_prev_selection, edges_selected)
  } else if (set_op == "intersect") {
    edges_combined <-
      intersect(edges_prev_selection, edges_selected)
  } else if (set_op == "difference") {
    edges_combined <-
      base::setdiff(edges_prev_selection, edges_selected)
  }

  # Filter `edges_df` to provide the correct esdf
  edges_combined <-
    graph$edges_df %>%
    dplyr::filter(id %in% edges_combined) %>%
    dplyr::select(edge = "id", "from", "to")

  # Add the edge ID values to the active selection
  # of edges in `graph$edge_selection`
  graph$edge_selection <- edges_combined

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

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
      version_id = nrow(graph$graph_log) + 1L,
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
    if (!n_e_select_properties_in[["node_selection_available"]] &&
        !n_e_select_properties_in[["edge_selection_available"]]) {

      msg_body <-
        glue::glue(
          "created a new selection of \\
        {n_e_select_properties_out[['selection_count_str']]}")

    } else if (n_e_select_properties_in[["node_selection_available"]] ||
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
