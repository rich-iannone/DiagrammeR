#' Select nodes in a graph
#'
#' Select nodes from a graph object of class \code{dgr_graph}.
#' @inheritParams render_graph
#' @param conditions an option to use filtering conditions for the retrieval of
#'   nodes.
#' @param set_op the set operation to perform upon consecutive selections of
#'   graph nodes. This can either be as a \code{union} (the default), as an
#'   intersection of selections with \code{intersect}, or, as a
#'   \code{difference} on the previous selection, if it exists.
#' @param nodes an optional vector of node IDs for filtering the list of nodes
#'   present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "z", "z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("a", "z", "a"))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select nodes `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(nodes = c(1, 3))
#'
#' # Verify that the node selection has been made
#' # using the `get_selection()` function
#' graph %>% get_selection()
#'
#' # Select nodes based on the node `type`
#' # being `z`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(
#'     conditions = type == "z")
#'
#' # Verify that an node selection has been made, and
#' # recall that the `3` and `4` nodes are of the
#' # `z` type
#' graph %>% get_selection()
#'
#' # Select edges based on the node value attribute
#' # being greater than 3.0 (first clearing the current
#' # selection of nodes)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(
#'     conditions = value > 3.0)
#'
#' # Verify that the correct node selection has been
#' # made; in this case, nodes `1` and `3` have values
#' # for `value` greater than 3.0
#' graph %>% get_selection()
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ get_expr
#' @export
select_nodes <- function(graph,
                         conditions = NULL,
                         set_op = "union",
                         nodes = NULL) {

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

  # Stop function if `nodes` refers to node ID
  # values that are not in the graph
  if (!is.null(nodes)) {
    if (!any(nodes %in% graph$nodes_df$id)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The values provided in `nodes` do not all correspond to node ID values in the graph")
    }
  }

  # Capture provided conditions
  conditions <- rlang::enquo(conditions)

  # Create binding for a specific variable
  id <- NULL

  # Extract the graph's internal ndf
  nodes_df <- graph$nodes_df

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `nodes_df`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    nodes_df <-
      filter(
        .data = nodes_df,
        rlang::UQ(conditions))
  }

  # Get the nodes as a vector
  nodes_selected <-
    nodes_df %>%
    dplyr::pull(id)

  # If a `nodes` vector provided, get the intersection
  # of that vector with the filtered node IDs
  if (!is.null(nodes)) {
    nodes_selected <- intersect(nodes, nodes_selected)
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  # Incorporate the selected nodes into the
  # graph's selection
  if (set_op == "union") {
    nodes_combined <- union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <- intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <- base::setdiff(nodes_prev_selection, nodes_selected)
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
          "modified an existing selection of \\
           {n_e_select_properties_in[['selection_count_str']]}:
           * {n_e_select_properties_out[['selection_count_str']]} \\
           are now in the active selection
           * used the `{set_op}` set operation")
    }

    if (n_e_select_properties_in[["edge_selection_available"]]) {
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

  graph
}
