#' Invert selection of nodes or edges in a graph
#'
#' @description
#'
#' Modify the selection of nodes or edges within a graph object such that all
#' nodes or edges previously not selected will now be selected and vice versa.
#'
#' @inheritParams render_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "standard")
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Select nodes with ID
#' # values `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     nodes = c(1, 3))
#'
#' # Verify that a node
#' # selection has been made
#' graph %>% get_selection()
#'
#' # Invert the selection
#' graph <-
#'   graph %>%
#'   invert_selection()
#'
#' # Verify that the node
#' # selection has been changed
#' graph %>% get_selection()
#'
#' @export
invert_selection <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph object has valid selection of
  # nodes or edges
  if (!graph_contains_node_selection(graph) &&
      !graph_contains_edge_selection(graph)) {

    cli::cli_abort(
      "There is no selection of nodes or edges available.")
  }

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # Invert the nodes in the selection
  if (nrow(graph$node_selection) > 0) {

    selection_nodes <- graph$node_selection$node

    ndf <- graph$nodes_df

    inverted_nodes <-
      ndf %>%
      dplyr::filter(!(id %in% selection_nodes)) %>%
      dplyr::select("id")

    # Add the node ID values to the active selection
    # of nodes in `graph$node_selection`
    graph$node_selection <-
      replace_graph_node_selection(
        graph = graph,
        replacement = inverted_nodes$id)

    # Replace `graph$edge_selection` with an empty df
    graph$edge_selection <- create_empty_esdf()
  }

  # Invert the edges in the selection
  if (nrow(graph$edge_selection) > 0) {

    selection_edges <- graph$edge_selection$edge

    edf <- graph$edges_df

    inverted_edges <-
      edf %>%
      dplyr::filter(!(id %in% selection_edges)) %>%
      dplyr::select("id", "from", "to")

    # Add the node ID values to the active selection
    # of nodes in `graph$node_selection`
    graph$edge_selection <-
      replace_graph_edge_selection(
        graph = graph,
        edge_id = inverted_edges$id,
        from_node = inverted_edges$from,
        to_node = inverted_edges$to)

    # Replace `graph$node_selection` with an empty df
    graph$node_selection <- create_empty_nsdf()
  }

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
    msg_body <-
      glue::glue(
        "inverted an existing selection of \\
       {n_e_select_properties_in[['selection_count_str']]}:
       * {n_e_select_properties_out[['selection_count_str']]} \\
       are now in the active selection")

    # Issue a message to the user
    emit_message(
      fcn_name = fcn_name,
      message_body = msg_body)
  }

  graph
}
