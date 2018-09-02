#' Copy an edge attribute column and set the name
#'
#' Within a graph's internal edge data frame (edf), copy the contents an
#'   existing edge attribute and create a distinct edge attribute within the edf
#'   with a different attribute name.
#' @inheritParams render_graph
#' @param edge_attr_from the name of the edge attribute column from which values
#'   will be copied.
#' @param edge_attr_to the name of the new edge attribute column to which the
#'   copied values will be placed.
#' @return a graph object of class \code{dgr_graph}.
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
#' # Get the graph's internal
#' # edf to show which edge
#' # attributes are available
#' graph %>%
#'   get_edge_df()
#'
#' # Make a copy the `color`
#' # edge attribute as the
#' # `color_2` edge attribute
#' graph <-
#'   graph %>%
#'   copy_edge_attrs(
#'     edge_attr_from = color,
#'     edge_attr_to = color_2)
#'
#' # Get the graph's internal
#' # edf to show that the edge
#' # attribute had been copied
#' graph %>%
#'   get_edge_df()
#' @importFrom dplyr bind_cols
#' @importFrom rlang enquo get_expr
#' @export
copy_edge_attrs <- function(graph,
                            edge_attr_from,
                            edge_attr_to) {

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
      reasons = "You cannot make a copy with the same name")
  }

  # Stop function if `edge_attr_to` is `from` or `to`
  if (any(c("from", "to") %in% edge_attr_to)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "You cannot use `from` or `to` as names.")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's column
  if (!any(column_names_graph %in% edge_attr_from)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The edge attribute to copy is not in the ndf")
  }

  # Get the column number for the edge attr to copy
  col_num_copy_from <-
    which(colnames(edges) %in% edge_attr_from)

  # Copy the column using `bind_cols()`
  edges <-
    dplyr::bind_cols(
      edges,
      as.data.frame(
        edges[, col_num_copy_from],
        stringsAsFactors = FALSE))

  # Set the column name for the copied attr
  colnames(edges)[ncol(edges)] <- edge_attr_to

  # Modify the new graph object
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
