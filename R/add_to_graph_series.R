#' Add graph object to a graph series object
#' Add a graph object to an extant graph series object for storage of multiple graphs across a sequential or temporal one-dimensional array.
#' @param graph a graph object to add to the graph series object
#' @param graph_series a graph series object to which the graph object will be added.
#' @return a graph series object of type 'dgr_graph_1D'.
#' @export add_to_graph_series

add_to_graph_series <- function(graph,
                                graph_series){

  # Get the series type
  series_type <- graph_series$series_type

  # Stop function if graph is not valid
  if (class(graph) != "dgr_graph"){
    return(graph_series)
  }

  # Stop function if graph series type is not valid
  if (!(series_type %in% c("sequential", "temporal"))){
    return(graph_series)
  }

  # Add graph to graph series
  graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

  return(graph_series)
}
