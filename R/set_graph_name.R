#' Set graph name
#'
#' @description
#'
#' Set a name for a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#' @param name The name to set for the graph.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a name
#' graph <-
#'   graph %>%
#'   set_graph_name(
#'     name = "example_name")
#'
#' @export
set_graph_name <- function(
    graph,
    name
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Set the graph's name
  graph$graph_info$graph_name[1] <-
    as.character(name)

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
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
