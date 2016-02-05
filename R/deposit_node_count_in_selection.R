#' Deposit a count of nodes (available in a selection) in the graph
#' @description From a graph object of class \code{dgr_graph}, get a
#' count of nodes available in a selection and deposit that value in
#' the graph for later retrieval using \code{withdraw_values}
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export deposit_node_count_in_selection

deposit_node_count_in_selection <- function(graph){

  # If no node selection is available, return the graph unchanged
  if (is.null(graph$selection$nodes)){

    return(graph)

  } else {

    # Place numeric vector of single length as a deposit in the graph
    graph$deposit <- length(graph$selection$nodes)

    return(graph)
  }
}
