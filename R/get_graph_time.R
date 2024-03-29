#' Get the graph date-time or timezone
#'
#' @description
#'
#' Get the time and timezone for a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#'
#' @return A single-length `POSIXct` vector with the assigned graph time.
#'
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
#' graph %>% get_graph_time()
#'
#' @export
get_graph_time <- function(graph) {
  check_graph_valid(graph)
  graph$graph_info$graph_time
}
