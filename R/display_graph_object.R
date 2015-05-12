#' Update and display graph object in Viewer
#' @description Using a 'gv_graph' object, update values of counts for nodes, edges, attributes, directed state, and display the schematic in the RStudio Viewer.
#' @param graph a 'gv_graph' object, created using the \code{create_graph} function
#' @import stringr
#' @export display_graph_object

display_graph_object <- function(graph){

  # Get updated counts of nodes in graph
  total_node_count <- node_count(graph = graph,
                                 type = FALSE)

}
