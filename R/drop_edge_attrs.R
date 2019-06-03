#' Drop an edge attribute column
#'
#' Within a graph's internal edge data frame (edf), remove an existing edge
#'   attribute.
#' @inheritParams render_graph
#' @param edge_attr the name of the edge attribute column to drop.
#' @return a graph object of class `dgr_graph`.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5,
#'     m = 6,
#'     set_seed = 23) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = 3) %>%
#'   mutate_edge_attrs(
#'     penwidth = value * 2)
#'
#' # Get the graph's internal
#' # edf to show which edge
#' # attributes are available
#' graph %>% get_edge_df()
#'
#' # Drop the `value` edge
#' # attribute
#' graph <-
#'   graph %>%
#'   drop_edge_attrs(
#'     edge_attr = value)
#'
#' # Get the graph's internal
#' # edf to show that the edge
#' # attribute `value` had been
#' # removed
#' graph %>% get_edge_df()
#' @importFrom rlang enquo get_expr UQ
#' @export
drop_edge_attrs <- function(graph,
                            edge_attr) {

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

  # Get the requested `edge_attr`
  edge_attr <-
    rlang::enquo(edge_attr) %>% rlang::get_expr() %>% as.character()

  # Stop function if `edge_attr` is any of
  # `from`, `to`, or `rel`
  if (any(c("from", "to", "rel") %in%
          edge_attr)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "You cannot drop this column")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr` is not one
  # of the graph's column
  if (!any(column_names_graph %in% edge_attr)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The edge attribute to drop is not in the ndf")
  }

  # Get the column number for the edge attr to drop
  col_num_drop <-
    which(colnames(edges) %in% edge_attr)

  # Remove the column
  edges <- edges[, -col_num_drop]

  # Update the graph object
  graph$edges_df <- edges

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
