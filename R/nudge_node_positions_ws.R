#' Move layout positions of a selection of nodes
#'
#' @description
#'
#' With an active selection of nodes, move the position in either the `x` or `y`
#' directions, or both. Nodes in the selection that do not have position
#' information (i.e., `NA` values for the `x` or `y` node attributes) will be
#' ignored.
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
#' @param dx A single numeric value specifying the amount that selected nodes
#'   (with non-`NA` values for the `x` and `y` attributes) will be moved in the
#'   x direction. A positive value will move nodes right, negative left.
#' @param dy A single numeric value specifying the amount that selected nodes
#'   (with non-`NA` values for the `x` and `y` attributes) will be moved in the
#'   y direction. A positive value will move nodes up, negative down.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a simple graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(
#'     type = "a",
#'     label = "one") %>%
#'   add_node(
#'     type = "a",
#'     label = "two") %>%
#'   add_node(
#'     type = "b",
#'     label = "three") %>%
#'   add_node(
#'     type = "b",
#'     label = "four")
#'
#' # Add position information to each of
#' # the graph's nodes
#' graph <-
#'   graph %>%
#'   set_node_position(
#'     node = 1, x = 1, y = 1) %>%
#'   set_node_position(
#'     node = 2, x = 2, y = 2) %>%
#'   set_node_position(
#'     node = 3, x = 3, y = 3) %>%
#'   set_node_position(
#'     node = 4, x = 4, y = 4)
#'
#' # Select all of the graph's nodes using the
#' # `select_nodes()` function (and only
#' # specifying the graph object)
#' graph <- select_nodes(graph)
#'
#' # Move the selected nodes (all the nodes,
#' # in this case) 5 units to the right
#' graph <-
#'   graph %>%
#'   nudge_node_positions_ws(
#'     dx = 5, dy = 0)
#'
#' # View the graph's node data frame
#' graph %>% get_node_df()
#'
#' # Now select nodes that have `type == "b"`
#' # and move them in the `y` direction 2 units
#' # (the graph still has an active selection
#' # and so it must be cleared first)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(
#'     conditions = type == "b") %>%
#'   nudge_node_positions_ws(
#'     dx = 0, dy = 2)
#'
#' # View the graph's node data frame
#' graph %>% get_node_df()
#'
#' @export
nudge_node_positions_ws <- function(
    graph,
    dx,
    dy
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of nodes available.")
  }

  # Get the graph's node data frame as an object
  ndf <- graph$nodes_df

  # If both the `x` and `y` attributes do not exist,
  # stop the function
  if (!("x" %in% colnames(ndf)) |
      !("y" %in% colnames(ndf))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There are no `x` and `y` attribute values to modify")
  }

  # Get the current selection of nodes
  nodes <- graph$node_selection$node

  # Determine which of the nodes selected have position
  # information set (i.e., not NA)
  ndf_filtered <-
    ndf %>%
    dplyr::filter(id %in% nodes) %>%
    dplyr::filter(!is.na(x) & !is.na(y))

  # If there are nodes to move, replace the `nodes`
  # vector with those node ID values; otherwise,
  # stop function
  if (nrow(ndf_filtered) == 0) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There are no nodes can be moved to different `x` or `y` locations")

  } else {
    nodes <- ndf_filtered$id
  }

  # Use `case_when` statements to selectively perform
  # a vectorized `if` statement across all nodes for
  # the `x` and `y` node attribute
  ndf_new <-
    ndf %>%
    dplyr::mutate(x = dplyr::case_when(
      id %in% as.integer(nodes) ~ x + dx,
      !(id %in% as.integer(nodes)) ~ x)) %>%
    dplyr::mutate(y = dplyr::case_when(
      id %in% nodes ~ y + dy,
      !(id %in% as.integer(nodes)) ~ y))

  # Replace the graph's node data frame with `ndf_new`
  graph$nodes_df <- ndf_new

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

  graph
}
