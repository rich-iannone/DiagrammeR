#' Set global graph attributes
#' @description Set global attributes
#' of a specific type (either
#' \code{graph_attrs}, \code{node_attrs},
#' or \code{edge_attrs} for a graph
#' object of class \code{dgr_graph}).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param attr the name of the
#' attribute to set for the \code{type}
#' of global attribute specified.
#' @param value the value to be set
#' for the chosen attribute specified
#' in the \code{attr_for_type} argument.
#' @param attr_type the specific type
#' of global graph attribute to set.
#' The type is specified with \code{graph},
#' \code{node}, or \code{edge}.
#' @return a graph object of
#' class \code{dgr_graph}.
#' @examples
#' # Create a new graph and set
#' # some global attributes
#' graph <-
#'   create_graph() %>%
#'   set_global_graph_attrs(
#'     attr = "overlap",
#'     value = "true",
#'     attr_type = "graph")
#'
#' # Verify that the global attributes
#' # have been set
#' graph %>%
#'   get_global_graph_attrs()
#' @importFrom dplyr tibble
#' @export set_global_graph_attrs

set_global_graph_attrs <- function(graph,
                                   attr,
                                   value,
                                   attr_type) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Ensure that the lengths of vectors for `attr`,
  # `value`, and `attr_type` are equivalent
  if ((length(attr) != length(value)) |
      (length(value) != length(attr_type)) |
      (length(attr) != length(attr_type))) {

    stop(
      "Vector lengths for `attr`, `value`, and `attr_type` must be equal.",
      call. = FALSE)
  }

  # Coerce any logical value for `value` to a
  # lowercase character value
  if (length(value) == 1) {
    if (inherits(value, "logical") & value %in% c(TRUE, FALSE)) {
      value <- tolower(as.character(value))
    }
  }

  # Create a table for the attributes
  global_attrs <-
    dplyr::tibble(
      attr = as.character(attr),
      value = as.character(value),
      attr_type = as.character(attr_type)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  graph$global_attrs <- global_attrs

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_global_graph_attrs",
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
