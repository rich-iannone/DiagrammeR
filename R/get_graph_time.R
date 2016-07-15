#' Get the graph date-time or timezone
#' @description Set the time and timezone for a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param as_posixct an option to return the graph time
#' as POSIXct.
#' @param get_tz an option to just return the graph's
#' time zone.
#' @examples
#' library(magrittr)
#'
#' # Create an empty graph and set the graph's time;
#' # if nothing is supplied for the `tz` argument,
#' # `GMT` is used as the time zone
#' graph <-
#'   create_graph() %>%
#'     set_graph_time(
#'       time = "2015-10-25 15:23:00")
#'
#' # Get the graph's time as a POSIXct-formatted object
#' # using `get_graph_time()`
#' graph %>% get_graph_time
#' #> [1] "2015-10-25 15:23:00 GMT"
#'
#' # Get the graph's time as a character object
#' graph %>% get_graph_time(as_posixct = FALSE)
#' #> [1] "2015-10-25 15:23:00"
#'
#' # Get just the time zone associated with the
#' # graph time
#' graph %>% get_graph_time(get_tz = TRUE)
#' #> [1] "GMT"
#' @return a graph object of class \code{dgr_graph}.
#' @export get_graph_time

get_graph_time <- function(graph,
                           as_posixct = TRUE,
                           get_tz = FALSE) {

  # If the graph has a time and time zone,
  # assign those to the `time` and `tz` objects
  if (!is.null(graph$graph_time) &
      !is.null(graph$graph_tz)) {
    time <- graph$graph_time
    tz <- graph$graph_tz
  }

  # If the graph does not have a time assigned,
  # return NA
  if (is.null(graph$graph_time)) {
   time <- NA
   tz <- NA
  }

  # If graph time requested as POSIXct, then
  # create the POSIXct object
  if (as_posixct == TRUE) {
    time <- as.POSIXct(time, tz = tz)
  }

  # If just the time zone is requested with
  # `get_tz == TRUE`, return the time zone, otherwise
  # return the graph time
  if (get_tz == TRUE) {
    return(tz)
  } else {
    return(time)
  }
}
