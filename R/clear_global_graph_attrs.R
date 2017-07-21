#' Clear any global graph attributes that are set
#' @description Clear any currently set global graph
#' attributes for a graph object of class
#' \code{dgr_graph}).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and set some global attributes
#' graph <-
#'   create_graph() %>%
#'   set_global_graph_attrs(
#'     attr = "overlap",
#'     value = "true",
#'     attr_type = "graph")
#'
#' # Clear all global attributes that have been set
#' graph <- clear_global_graph_attrs(graph)
#'
#' # Look at the present global graph attributes;
#' # since there are none, NA is returned
#' get_global_graph_attrs(graph)
#' #> [1] NA
#' @export clear_global_graph_attrs

clear_global_graph_attrs <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Clear the global graph attributes data frame
  # by removing all rows from it
  graph$global_attrs <-
    graph$global_attrs[-(1:(nrow(graph$global_attrs))), ]

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "clear_global_graph_attrs",
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
