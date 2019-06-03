#' Select edges in a graph
#'
#' Select edges from a graph object of class `dgr_graph`.
#' @inheritParams render_graph
#' @param conditions an option to use filtering conditions for the retrieval of
#'   edges.
#' @param set_op the set operation to perform upon consecutive selections of
#'   graph nodes. This can either be as a `union` (the default), as an
#'   intersection of selections with `intersect`, or, as a
#'   `difference` on the previous selection, if it exists.
#' @param from an optional vector of node IDs from which the edge is outgoing
#'   for filtering the list of edges present in the graph.
#' @param to an optional vector of node IDs to which the edge is incoming for
#'   filtering the list of edges present in the graph.
#' @param edges an optional vector of edge IDs for filtering the list of edges
#'   present in the graph.
#' @return a graph object of class `dgr_graph`.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("a", "z", "a"),
#'     value = c(6.4, 2.9, 5.0))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select the edge `1`->`4`
#' graph <-
#'   graph %>%
#'   select_edges(
#'     from = 1,
#'     to = 4)
#'
#' # Verify that an edge selection has been made
#' # using the `get_selection()` function
#' graph %>% get_selection()
#'
#' # Select edges based on the relationship label
#' # being `z`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges(
#'     conditions = rel == "z")
#'
#' # Verify that an edge selection has been made, and
#' # recall that the `2`->`3` edge uniquely has the
#' # `z` relationship label
#' graph %>% get_selection()
#'
#' # Select edges based on the edge value attribute
#' # being greater than 3.0 (first clearing the current
#' # selection of edges)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges(
#'     conditions = value > 3.0)
#'
#' # Verify that the correct edge selection has been
#' # made; in this case, edges `1`->`4` and
#' # `3`->`1` have values for `value` > 3.0
#' graph %>% get_selection()
#' @importFrom dplyr filter select rename
#' @importFrom rlang enquo get_expr UQ
#' @export
select_edges <- function(graph,
                         conditions = NULL,
                         set_op = "union",
                         from = NULL,
                         to = NULL,
                         edges = NULL) {

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

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Stop function if `edges` refers to edge ID
  # values that are not in the graph
  if (!is.null(edges)) {
    if (!any(edges %in% graph$edges_df$id)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The values provided in `edges` do not all correspond to edge ID values in the graph")
    }
  }

  # Capture provided conditions
  conditions <- rlang::enquo(conditions)

  # Extract the graph's internal edf
  edges_df <- graph$edges_df

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `edges_df`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    edges_df <-
      dplyr::filter(
        .data = edges_df,
        UQ(conditions))
  }

  # If a `from` vector provided, filter the edf
  # to get those edges where the specified node IDs
  # are present
  if (!is.null(from)) {
    if (any(!(from %in% edges_df$from))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "One of more of the nodes specified as `from` not part of an edge")
    }

    from_val <- from

    edges_df <-
      edges_df %>%
      dplyr::filter(from %in% from_val)
  }

  # If a `to` vector provided, filter the edf
  # to get those edges where the specified node IDs
  # are present
  if (!is.null(to)) {
    if (any(!(to %in% edges_df$to))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "One of more of the nodes specified as `to` are not part of an edge")
    }

    to_val <- to

    edges_df <-
      edges_df %>%
      dplyr::filter(to %in% to_val)
  }

  # Select only the `id`, `to`, and `from` columns
  edges_selected <-
    edges_df %>%
    dplyr::select(id, from, to) %>%
    dplyr::rename(edge = id)

  # Create an integer vector representing edges
  edges_selected <- edges_selected$edge

  # If an `edges` vector provided, get the intersection
  # of that vector with the filtered edge IDs
  if (!is.null(edges)) {
    edges_selected <- intersect(edges, edges_selected)
  }

  # Obtain vector with edge ID selection of edges
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
    dplyr::select(id, from, to) %>%
    dplyr::rename(edge = id)

  # Add the edge ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$edge_selection <- edges_combined

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

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
            "modified an existing selection of \\
           {n_e_select_properties_in[['selection_count_str']]}:
           * {n_e_select_properties_out[['selection_count_str']]} \\
           are now in the active selection
           * used the `{set_op}` set operation")
      }

      if (n_e_select_properties_in[["node_selection_available"]]) {
        msg_body <-
          glue::glue(
            "created a new selection of \\
           {n_e_select_properties_out[['selection_count_str']]}:
           * this replaces \\
           {n_e_select_properties_in[['selection_count_str']]} \\
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
