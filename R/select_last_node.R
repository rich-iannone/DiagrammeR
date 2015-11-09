#' Select last node in a series of node IDs in a graph
#' @description Select the last node from a graph object of class
#' \code{dgr_graph}. Strictly, this is the node encompassing the last
#' record of the graph's node data frame. In practice, this will
#' typically be the last node added to the graph.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_last_node

select_last_node <- function(graph){

  if (is_graph_empty(graph)){
    stop("The graph is empty so no selections can be made.")
  }

  nodes <- graph$nodes_df$nodes

  last_node <- nodes[length(nodes)]

  graph$selection$nodes <- last_node

  return(graph)
}
