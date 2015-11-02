#' Get global graph attributes
#' @description Get the presently set global attributes for a graph object of
#' class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a list object containing global attributes for a graph.
#' @examples
#' \dontrun{
#' }
#' @export get_global_graph_attr

get_global_graph_attr <- function(graph){

    return(list(graph_attrs = graph$graph_attrs,
                node_attrs = graph$node_attrs,
                edge_attrs = graph$edge_attrs))
}
