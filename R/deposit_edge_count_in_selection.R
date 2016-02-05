#' Deposit a count of edges (available in a selection) in the graph
#' @description From a graph object of class \code{dgr_graph}, get a
#' count of edges available in a selection and deposit that value in
#' the graph for later retrieval using \code{withdraw_values}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export deposit_edge_count_in_selection

deposit_edge_count_in_selection <- function(graph){

  # If no edge selection is available, return the graph unchanged
  if (is.null(graph$selection$edges)){

    return(graph)

  } else {

    # Place numeric vector of single length as a deposit in the graph
    graph$deposit <- length(graph$selection$edges$from)

    return(graph)
  }
}
