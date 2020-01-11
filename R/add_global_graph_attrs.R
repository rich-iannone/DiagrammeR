#' Add one or more global graph attributes
#'
#' Add global attributes of a specific type (either `graph_attrs`, `node_attrs`,
#' or `edge_attrs` for a graph object of class `dgr_graph`).
#'
#' @inheritParams render_graph
#' @param attr The name of the attribute to set for the `type` of global
#'   attribute specified.
#' @param value The value to be set for the chosen attribute specified in the
#'   `attr_for_type` argument.
#' @param attr_type The specific type of global graph attribute to set. The type
#'   is specified with `graph`, `node`, or `edge`.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a new graph with no
#' # global graph attributes and
#' # add a global graph attribute
#' graph <-
#'   create_graph(
#'     attr_theme = NULL) %>%
#'   add_global_graph_attrs(
#'     attr = "overlap",
#'     value = "true",
#'     attr_type = "graph")
#'
#' # Verify that the attribute
#' # addition has been made
#' graph %>%
#'   get_global_graph_attr_info()
#'
#' # Add another attribute with
#' # `add_global_graph_attrs()`
#' graph <-
#'   graph %>%
#'   add_global_graph_attrs(
#'     attr = "penwidth",
#'     value = 12,
#'     attr_type = "node")
#'
#' # Verify that the attribute
#' # addition has been made
#' graph %>%
#'   get_global_graph_attr_info()
#'
#' # When adding an attribute where
#' # `attr` and `attr_type` already
#' # exists, the value provided will
#' # serve as an update
#' graph %>%
#'   add_global_graph_attrs(
#'     attr = "penwidth",
#'     value = 15,
#'     attr_type = "node") %>%
#'   get_global_graph_attr_info()
#'
#' @export
add_global_graph_attrs <- function(graph,
                                   attr,
                                   value,
                                   attr_type) {

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

  # Coerce any logical value for `value` to a
  # lowercase character value
  if (length(value) == 1) {
    if (inherits(value, "logical") &
        value %in% c(TRUE, FALSE)) {
      value <- tolower(as.character(value))
    }
  }

  # Create a table for the attributes
  global_attrs_to_add <-
    dplyr::tibble(
      attr = as.character(attr),
      value = as.character(value),
      attr_type = as.character(attr_type)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # Get the global graph attributes already set
  # in the graph object
  global_attrs_available <- graph$global_attrs

  # Join the new attributes to those available
  # on the `attr` and `attr_type` columns
  global_attrs_joined <-
    global_attrs_available %>%
    dplyr::full_join(
      global_attrs_to_add,
      by = c("attr", "attr_type")) %>%
    dplyr::transmute(
      attr, attr_type,
      value = dplyr::coalesce(value.y, value.x)) %>%
    dplyr::select(attr, value, attr_type)

  # Replace the graph's global attributes with
  # the revised set
  graph$global_attrs <- global_attrs_joined

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
