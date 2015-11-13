#' Delete all selected nodes
#' @description In a graph object of class \code{dgr_graph}, delete
#' all nodes present in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export delete_nodes_in_selection

delete_nodes_in_selection <- function(graph){

  # If no node selection is available, return the graph unchanged
  if (is.null(graph$selection$nodes)){
    return(graph)
  }

  # Get a vector of the nodes to be deleted
  nodes_to_delete <- graph$selection$nodes

  # Delete all nodes in `nodes_to_delete`
  for (i in 1:length(nodes_to_delete)){

    graph <-
      delete_node(graph = graph,
                  node = nodes_to_delete[i])
  }

  # Remove all nodes in selection
  graph$selection$nodes <- NULL

  return(graph)
}
