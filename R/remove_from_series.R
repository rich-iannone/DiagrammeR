#' Remove graph object from a graph series object
#' @description Remove a single graph object from an array of graph objects in a graph series object.
#' @param graph_series a graph series object from which the graph object will be removed.
#' @param index the index of the graph object to be removed from the graph series object.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @export remove_from_series

remove_from_series <- function(graph_series,
                               index = "last"){

  if (index == "last"){
    graph_series$graphs[[length(graph_series$graphs)]] <- NULL

    return(graph_series)
  }

  if (index == "first"){
    graph_series$graphs[[1]] <- NULL

    return(graph_series)
  }

  if (class(index) == "numeric" | class(index) == "integer"){
    graph_series$graphs[[index]] <- NULL

    return(graph_series)
  }
}
