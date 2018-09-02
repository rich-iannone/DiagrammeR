#' Add a clone of an existing edge to the graph
#'
#' Add a new edge to a graph object of class \code{dgr_graph} which is a clone
#'   of an edge already in the graph. All edge attributes are preserved.
#' @inheritParams render_graph
#' @param edge an edge ID corresponding to the graph edge to be cloned.
#' @param from the outgoing node from which the edge is connected.
#' @param to the incoming nodes to which each edge is connected.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a path of
#' # 2 nodes; supply a common `rel`
#' # edge attribute for all edges
#' # in this path and then add a
#' # `color` edge attribute
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 2,
#'     rel = "a") %>%
#'   select_last_edges_created() %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "steelblue") %>%
#'   clear_selection()
#'
#' # Display the graph's internal
#' # edge data frame
#' graph %>%
#'   get_edge_df()
#'
#' # Create a new node (will have
#' # node ID of `3`) and then
#' # create an edge between it and
#' # node `1` while reusing the edge
#' # attributes of edge `1` -> `2`
#' # (edge ID `1`)
#' graph_2 <-
#'   graph %>%
#'   add_node() %>%
#'   add_edge_clone(
#'     edge = 1,
#'     from = 3,
#'       to = 1)
#'
#' # Display the graph's internal
#' # edge data frame
#' graph_2 %>%
#'   get_edge_df()
#'
#' # The same change can be performed
#' # with some helper functions in the
#' # `add_edge_clone()` function call
#' graph_3 <-
#'   graph %>%
#'     add_node() %>%
#'     add_edge_clone(
#'       edge = get_last_edges_created(.),
#'       from = get_last_nodes_created(.),
#'       to = 1)
#'
#' # Display the graph's internal
#' # edge data frame
#' graph_3 %>%
#'   get_edge_df()
#' @importFrom dplyr filter select
#' @export
add_edge_clone <- function(graph,
                           edge,
                           from,
                           to) {

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

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Stop function if edge is not a single numerical value
  if (length(edge) > 1 | inherits(edge, "character") | inherits(edge, "logical")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value for `edge` must be a single, numeric value")
  }

  # Stop function the edge ID does not correspond
  # to an edge in the graph
  if (!(edge %in% graph$edges_df$id)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value provided in `edge` does not correspond to an edge in the graph")
  }

  # Create bindings for specific variables
  id <- version_id <- NULL

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    graph$graph_log$version_id %>%
    max()

  # Get the number of columns in the graph's
  # internal edge data frame
  n_col_edf <-
    graph %>%
    get_edge_df() %>%
    ncol()

  # Extract all of the edge attributes
  # (`rel` and additional edge attrs)
  edge_attr_vals <-
    graph %>%
    get_edge_df() %>%
    dplyr::filter(id == edge) %>%
    dplyr::select(4:n_col_edf)

  # Create the requested edge
  graph <-
    graph %>%
    add_edge(
      from = from,
      to = to)

  # Create an edge selection for the
  # new edge in the graph
  graph <-
    graph %>%
    select_last_edges_created()

  # Iteratively set edge attribute values for
  # the new edges in the graph
  for (i in 1:ncol(edge_attr_vals)) {

    graph$edges_df[
      nrow(graph$edges_df),
      which(colnames(graph$edges_df) == colnames(edge_attr_vals)[i])] <-
      edge_attr_vals[[i]]
  }

  # Clear the graph's active selection
  graph <-
    suppressMessages(
      graph %>%
        clear_selection())

  # Remove extra items from the `graph_log`
  graph$graph_log <-
    graph$graph_log %>%
    dplyr::filter(version_id <= current_graph_log_version_id)

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
      d_e = 1)

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
