#' Create a graph series object
#' Create a graph series object for storage of multiple graphs across a sequential or temporal one-dimensional array.
#' @param graph a graph object to add to the new graph series object.
#' @param graph_name an optional string for labeling the graph added to the graph series.
#' @param graph_time a date or date-time string which is required for a graph series of the type 'temporal'.
#' @param graph_tz an optional value for the time zone (tz) value corresponding to the date or date-time string supplied as a value to 'graph_time'. If no time zone is provided then it will be set to 'GMT'.
#' @param series_name an optional name to ascribe to the series.
#' @param series_type either a 'sequential' type (the default) or a 'temporal' type (which requires date-time strings and time zone codes to be supplied).
#' @return a graph series object of type 'dgr_graph_1D'.
#' @export create_graph_series

create_graph_series <- function(graph = NULL,
                                graph_name = NULL,
                                graph_time = NULL,
                                graph_tz = NULL,
                                series_name = NULL,
                                series_type = "sequential"){

  # Initialize an empty graph series object
  graph_series <- list(graphs = NULL, graph_name = NULL,
                       graph_time = NULL, graph_tz = NULL,
                       series_name = series_name, series_type = series_type)
  attr(graph_series, "class") <- "dgr_graph_1D"

  if (is.null(graph)){
    return(graph_series)
  }

  # Add a graph to the initialized graph series
    graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

  return(graph_series)
}
