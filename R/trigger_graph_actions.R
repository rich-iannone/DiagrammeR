#' Trigger the execution of a series of graph actions
#' @description Execute the graph actions stored in the
#' graph through the use of the \code{add_graph_action()}
#' function. These actions will be invoked in order and
#' any errors encountered will trigger a warning message
#' and result in no change to the input graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @importFrom dplyr filter pull
#' @export trigger_graph_actions

trigger_graph_actions <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  if (nrow(graph$graph_actions) == 0) {

    message("There are currently no graph actions.")
  } else {

    # Collect text expressions in a vector
    graph_actions <- graph$graph_actions$expression

    # Copy graph state as the `graph_previous` object
    graph_previous <- graph

    expr_error_at_index <- 0

    for (i in 1:length(graph_actions)) {

      if (class(
        tryCatch(
          eval(
            parse(text = graph$graph_actions$expression[i])),
          error = function(x) x))[1] == "simpleError") {
        expr_error_at_index <- i
        break
      } else {
        graph <-
          eval(parse(text = graph$graph_actions$expression[i]))
      }
    }

    if (expr_error_at_index > 0) {

      # Revert `graph_previous` to be the returned
      # graph (because of an evaluation error)
      graph <- graph_previous

      action_name_at_error <-
        graph$graph_actions %>%
        dplyr::filter(action_index == expr_error_at_index) %>%
        dplyr::pull(action_name)

      if (!is.na(action_name_at_error)) {
        message(
          paste0(
            "The series of graph actions was not applied to the graph because ",
            "of an error at action index ", expr_error_at_index, "."))
      } else {
        message(
          paste0(
            "The series of graph actions was not applied to the graph because ",
            "of an error at action index ", expr_error_at_index, " (`",
            action_name_at_error, "`)."))
      }
    }

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "trigger_graph_actions",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

    # Write graph backup if the option is set
    if (graph$graph_info$write_backups) {
      save_graph_as_rds(graph = graph)
    }
  }

  return(graph)
}
