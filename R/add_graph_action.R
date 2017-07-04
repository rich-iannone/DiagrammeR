#' Add a graph action for execution at every transform
#' @description Add a graph function along with its
#' arguments to be run at every graph transformation
#' step.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param fcn the name of the function to use.
#' @param ... arguments and values to pass to
#' the named function in \code{fcn}, if necessary.
#' @param action_name an optional name for labeling
#' the action.
#' @return a graph object of class \code{dgr_graph}.
#' @importFrom dplyr bind_rows
#' @export add_graph_action

add_graph_action <- function(graph,
                             fcn,
                             ...,
                             action_name = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Collect any function arguments into the
  # `fcn_args` list object
  fcn_args <- list(...)

  # Create a character expression for the
  # function to evaluate at every graph
  # transformation step
  char_expr <-
    paste0(
      fcn,
      "(graph = graph, ",
      paste(fcn_args %>% names(),
            "=", paste(fcn_args %>% unname()),
            collapse = ", "),
      ")")

  # Create a data frame row with the new graph action
  new_graph_action <-
    data.frame(
      action_id = ifelse(nrow(graph$graph_actions) == 0, 1,
                         max(graph$graph_actions$action_id) + 1),
      action_name = ifelse(!is.null(action_name), action_name,
                           as.character(NA)),
      expression = char_expr,
      stringsAsFactors = FALSE)

  # Append `new_graph_action` to `graph$graph_actions`
  graph$graph_actions <-
    dplyr::bind_rows(graph$graph_actions, new_graph_action)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_graph_action",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
