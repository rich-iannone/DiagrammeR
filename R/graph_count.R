#' Count graphs in a graph series object
#' Counts the total number of graphs in a graph series object.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}
#' @return a numeric vector representing a count of graphs in a graph series object.
#' @export graph_count

graph_count <- function(graph_series){

  if (class(graph_series) == "dgr_graph_1D"){

    if (is.null(graph_series$graphs)){

      return(0)
    }

    return(length(graph_series$graphs))
  }
}
