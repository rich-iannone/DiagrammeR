#' Rename an edge attribute
#'
#' @description
#'
#' Within a graph's internal edge data frame (edf), rename an existing edge
#' attribute.
#'
#' @inheritParams render_graph
#' @param edge_attr_from The name of the edge attribute that will be renamed.
#' @param edge_attr_to The new name of the edge attribute column identified in
#'   `edge_attr_from`.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5,
#'     m = 8,
#'     set_seed = 23) %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "green")
#'
#' # Get the graph's internal edf
#' # to show which edge attributes
#' # are available
#' graph %>% get_edge_df()
#'
#' # Rename the `color` node
#' # attribute as `weight`
#' graph <-
#'   graph %>%
#'   rename_edge_attrs(
#'     edge_attr_from = color,
#'     edge_attr_to = labelfontcolor)
#'
#' # Get the graph's internal
#' # edf to show that the edge
#' # attribute had been renamed
#' graph %>% get_edge_df()
#'
#' @family Edge creation and removal
#'
#' @export
rename_edge_attrs <- function(
    graph,
    edge_attr_from,
    edge_attr_to
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

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Get the requested `edge_attr_from`
  edge_attr_from <-
    rlang::enquo(edge_attr_from) %>% rlang::get_expr() %>% as.character()

  # Get the requested `edge_attr_to`
  edge_attr_to <-
    rlang::enquo(edge_attr_to) %>% rlang::get_expr() %>% as.character()

  # Stop function if `edge_attr_from` and
  # `edge_attr_to` are identical
  if (edge_attr_from == edge_attr_to) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "You cannot rename using the same name")
  }

  # Stop function if `edge_attr_to` is `from`, `to`,
  # or any other column name in the graph's edge
  # data frame
  if (any(colnames(get_edge_df(graph)) %in%
          edge_attr_to)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "You cannot use that name for `edge_attr_to`")
  }

  # Stop function if `edge_attr_from` is `from`, `to`
  # or `rel`
  if (any(c("from", "to", "rel") %in%
          edge_attr_from)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "You cannot use that name for `edge_attr_from`")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's columns
  if (!any(column_names_graph %in% edge_attr_from)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The edge attribute to rename is not in the edf")
  }

  # Set the column name for the renamed attr
  colnames(edges)[
    which(colnames(edges) %in%
            edge_attr_from)] <- edge_attr_to

  # Modify the graph's edf
  graph$edges_df <- edges

  # Update the `last_node` counter
  graph$last_node <- nodes_created

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
