#' Get node attributes based on a selection of nodes
#' @description From a graph object of class \code{dgr_graph}, get node
#' attribute properties for nodes available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export get_node_attr_from_selection

get_node_attr_from_selection <- function(graph){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  nodes_df <- get_node_attr(graph, graph$selection$nodes)

  return(nodes_df)
}
