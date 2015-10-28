#' Invert selection of nodes or edges in a graph
#' @description Modify the selection of nodes or edges within a graph
#' object such that all nodes or edges previously unselected will now be
#' selected and vice versa.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export invert_selection

invert_selection <- function(graph){

  # Stop function if the graph does not contain a selection
  if (is.null(graph$selection)){
    stop("The graph does not contain an active selection")
  }

  # Filter the nodes in the graph
  if (!is.null(graph$selection$nodes)){

    selection_nodes <- graph$selection$nodes

    graph$selection <-
      get_nodes(graph)[which(!(get_nodes(graph) %in% selection_nodes))]

  }

  # Return the graph
  return(graph)
}
