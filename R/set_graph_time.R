#' Set graph date-time and timezone
#'
#' Set the time and timezone for a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#' @param time The date-time to set for the graph.
#' @param tz The timezone to set for the graph.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a timestamp (if `tz`
#' # is not supplied, `GMT` is used as the time zone)
#' graph_1 <-
#'   graph %>%
#'   set_graph_time(time = "2015-10-25 15:23:00")
#'
#' # Provide the new graph with a timestamp that is
#' # the current time; the time zone is inferred from
#' # the user's locale
#' graph_2 <-
#'   graph %>%
#'   set_graph_time()
#'
#' # The time zone can be updated when a timestamp
#' # is present
#' graph_2 <-
#'   graph_2 %>%
#'   set_graph_time(tz = "America/Los_Angeles")
#'
#' @export
set_graph_time <- function(graph,
                           time = NULL,
                           tz = NULL) {

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

  if (is.null(time) & is.null(tz)) {
    time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    tz <- Sys.timezone()
  }

  if (inherits(time[1], "POSIXct")) {

    if (!is.null(attr(time, "tzone"))) {
      tz <- attr(time, "tzone")
    }

    time <- format(time, "%Y-%m-%d %H:%M:%S")
  }

  if (!is.null(tz)) {
    if (!(tz %in% OlsonNames())) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The time zone provided must be available in `OlsonNames()`")
    }
  }

  if (is.null(tz) & is.null(graph$graph_tz)) {
    tz <- "GMT"
  }

  # Modify the graph's time and time zone attributes
  graph$graph_info$graph_time[1] <- time
  graph$graph_info$graph_tz[1] <- tz

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
