#' Get a count of edges available in a selection
#' @description From a graph object of class \code{dgr_graph}, get a
#' count of edges available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export get_edge_count_in_selection

get_edge_count_in_selection <- function(graph){

  # If no edge selection is available, return the graph unchanged
  if (is.null(graph$selection$edges)){

    return(graph)

  } else {

    # Place numeric vector of single length as a deposit in the graph
    graph$deposit <- length(graph$selection$edges$from)

    return(graph)
  }
}
