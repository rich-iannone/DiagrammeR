#' Get the graph date-time or timezone
#'
#' Get the time and timezone for a graph object of class \code{dgr_graph}.
#' @inheritParams render_graph
#' @return a single-length \code{POSIXct} vector with the assigned graph time.
#' @examples
#' # Create an empty graph and
#' # set the graph's time; if nothing
#' # is supplied for the `tz` argument,
#' # `GMT` is used as the time zone
#' graph <-
#'   create_graph() %>%
#'     set_graph_time(
#'       time = "2015-10-25 15:23:00")
#'
#' # Get the graph's time as a POSIXct
#' # object using `get_graph_time()`
#' graph %>%
#'   get_graph_time()
#' @export
get_graph_time <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  graph$graph_info$graph_time
}
