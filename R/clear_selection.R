#' Clear a selection of nodes or edges in a graph
#' @description Clear the selection of nodes or edges within a graph
#' object.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export clear_selection

clear_selection <- function(graph){

  # Clear the selection in the graph, if it exists
  if (!is.null(graph$selection)){
    graph$selection <- NULL
  }

  # Return the graph
  return(graph)
}
