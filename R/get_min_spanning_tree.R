#' Get a minimum spanning tree subgraph
#' @description Get a minimum spanning tree subgraph
#' for a connected graph of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @importFrom igraph mst
#' @export get_min_spanning_tree

get_min_spanning_tree <- function(graph) {

  # Transform the graph to an igraph object
  igraph <- to_igraph(graph)

  # Get the minimum spanning tree
  igraph_mst <- mst(igraph)

  # Generate the graph object from an igraph graph
  graph <- from_igraph(igraph_mst)

  return(graph)
}
