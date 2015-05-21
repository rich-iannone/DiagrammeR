#' Count graphs in a graph series object
#' Counts the total number of graphs in a graph series object.
#' @param graph_series a a graph series object.
#' @return a numeric vector representing a count of graphs in a graph series object.
#' @export graph_count

graph_count <- function(graph_series){

  if (class(graph_series) == "dgr_graph_1D"){

    return(length(series$graphs))
  }
}
