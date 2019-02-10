#' Select nodes in the graph based on their degree values
#'
#' Using a graph object of class \code{dgr_graph}, create a selection of nodes
#'   that have certain degree values.
#' @inheritParams render_graph
#' @param expressions one or more expressions for filtering nodes by degree
#'   values. Use a combination of degree type (\code{deg} for total degree,
#'   \code{indeg} for in-degree, and \code{outdeg} for out-degree) with a
#'   comparison operator and values for comparison (e.g., use \code{"deg >= 2"}
#'   to select nodes with a degree greater than or equal to 2).
#' @param set_op the set operation to perform upon consecutive selections of
#'   graph nodes. This can either be as a \code{union} (the default), as an
#'   intersection of selections with \code{intersect}, or, as a
#'   \code{difference} on the previous selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph using
#' # the `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 35, m = 125,
#'     set_seed = 23)
#'
#' # Report which nodes have a
#' # total degree (in-degree +
#' # out-degree) of exactly 9
#' graph %>%
#'   select_nodes_by_degree(
#'     expressions = "deg == 9") %>%
#'   get_selection()
#'
#' # Report which nodes have a
#' # total degree greater than or
#' # equal to 9
#' graph %>%
#'   select_nodes_by_degree(
#'     expressions = "deg >= 9") %>%
#'   get_selection()
#'
#' # Combine two calls of
#' # `select_nodes_by_degree()` to
#' # get those nodes with total
#' # degree less than 3 and total
#' # degree greater than 10 (by
#' # default, those `select...()`
#' # functions will `union` the
#' # sets of nodes selected)
#' graph %>%
#'   select_nodes_by_degree(
#'     expressions = "deg < 3") %>%
#'   select_nodes_by_degree(
#'     expressions = "deg > 10") %>%
#'   get_selection()
#'
#' # Combine two calls of
#' # `select_nodes_by_degree()` to
#' # get those nodes with total
#' # degree greater than or equal
#' # to 3 and less than or equal
#' # to 10 (the key here is to
#' # `intersect` the sets of nodes
#' # selected in the second call)
#' graph %>%
#'   select_nodes_by_degree(
#'     expressions = "deg >= 3") %>%
#'   select_nodes_by_degree(
#'     expressions = "deg <= 10",
#'     set_op = "intersect") %>%
#'   get_selection()
#'
#' # Select all nodes with an
#' # in-degree greater than 5, then,
#' # apply a node attribute to those
#' # selected nodes (coloring the
#' # selected nodes red)
#' graph_2 <-
#'   graph %>%
#'   select_nodes_by_degree(
#'     expressions = "indeg > 5") %>%
#'   set_node_attrs_ws(
#'     node_attr = color,
#'     value = "red")
#'
#' # Get the selection of nodes
#' graph_2 %>% get_selection()
#' @importFrom dplyr select filter_
#' @export
select_nodes_by_degree <- function(graph,
                                   expressions,
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

  # Obtain the input graph's node and edge
  # selection properties
  n_e_select_properties_in <-
    node_edge_selection_properties(graph = graph)

  # Create bindings for specific variables
  id <- deg <- indeg <- outdeg <- NULL

  # Get a data frame with node ID and degree types
  node_degree <-
    get_node_info(graph) %>%
    dplyr::select(id, deg, indeg, outdeg)

  for (i in 1:length(expressions)) {
    node_degree <-
      node_degree %>%
      dplyr::filter_(expressions[i])
  }

  # Get the node ID values from the filtered table
  nodes_selected <- node_degree$id

  # If no node ID values in `nodes_selected` return
  # the graph without a changed node selection
  if (length(nodes_selected) == 0) {
    return(graph)
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  # Incorporate selected nodes into graph's
  # selection section
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
  if (graph$graph_info$display_msgs) {

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
