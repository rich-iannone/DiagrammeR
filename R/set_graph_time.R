#' Set graph date-time and timezone
#' @description Set the time and timezone for a graph object of class
#' \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param time the date-time to set for the graph.
#' @param tz the timezone to set for the graph.
#' @examples
#' \dontrun{
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a timestamp (if `tz` not supplied,
#' # `GMT` is used as the time zone)
#' graph_1 <-
#'   set_graph_time(graph,
#'                  time = "2015-10-25 15:23:00")
#'
#' # Provide the new graph with a timestamp that is the current
#' # time; the time zone is inferred from the user's locale
#' graph_2 <- set_graph_time(graph)
#'
#' # The time zone can be updated when a timestamp is present
#' graph_2 <-
#'   set_graph_time(graph_2,
#'                  tz = "America/Los_Angeles")
#' }
#' @return a graph object of class \code{dgr_graph}.
#' @export set_graph_time

set_graph_time <- function(graph,
                           time = NULL,
                           tz = NULL){

  if (is.null(time) & is.null(tz)){
    time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    tz <- Sys.timezone()
  }

  if (class(time)[1] == "POSIXct"){

    if (!is.null(attr(time, "tzone"))){
      tz <- attr(time, "tzone")
    }

    time <- format(time, "%Y-%m-%d %H:%M:%S")
  }

  if (!is.null(tz)){
    if (!(tz %in% OlsonNames())){
      stop("The time zone provided must be available in `OlsonNames()`.")
    }
  }

  if (is.null(tz) & is.null(graph$graph_tz)){
    tz <- "GMT"
  }

  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = graph$nodes_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = ifelse(is_graph_directed(graph),
                                   TRUE, FALSE),
                 graph_name = graph$graph_name,
                 graph_time = ifelse(!is.null(time),
                                     time, graph$graph_time),
                 graph_tz = tz)

  if (!is.null(graph$selection)){
    dgr_graph$selection <- graph$selection
  }

  return(dgr_graph)
}
