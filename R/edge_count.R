#' Get count of all edges or edges with distinct relationship types
#' @description From a graph object of class 'gv_graph', get a count of edges in the graph and optionally obtain a count of edges by their relationship type.
#' @param graph a graph object of class 'gv_graph' that is created using 'create_graph'.
#' @param relationship either a logical value, where TRUE provides a named vector of edge count by type and FALSE (the default) provides a total count of edges, or, a string corresponding to one or more edge relationship types.
#' @return a numeric vector of single length
#' @export edge_count

edge_count <- function(graph,
                       relationship = FALSE){


}
