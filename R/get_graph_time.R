#' Get the graph date-time or timezone
#' @description Set the time and timezone for a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and set the graph's time;
#' # if nothing is supplied for the `tz` argument,
#' # `GMT` is used as the time zone
#' graph <-
#'   create_graph() %>%
#'     set_graph_time(
#'       time = "2015-10-25 15:23:00")
#'
#' # Get the graph's time as a POSIXct
#' # object using `get_graph_time()`
#' graph %>% get_graph_time()
#' #> [1] "2015-10-25 15:23:00 GMT"
#' @export get_graph_time

get_graph_time <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  return(graph$graph_info$graph_time)
}
