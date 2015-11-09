#' Select last edge in a series of edges defined in a graph
#' @description Select the last edge from a graph object of class
#' \code{dgr_graph}. Strictly, this is the edge definition that
#' encompasses the last record of the graph's edge data frame. In
#' practice, this will typically be the last edge created.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_last_edge

select_last_edge <- function(graph){

  if (is_graph_empty(graph)){
    stop("The graph is empty so no selections can be made.")
  }

  if (edge_count(graph) == 0){
    stop("The graph has no edges so no selections can be made.")
  }

  from <- graph$edges_df$from
  to <- graph$edges_df$to

  last_from <- from[length(from)]
  last_to <- to[length(to)]

  graph$selection$edges$from <- last_from
  graph$selection$edges$to <- last_to

  if (!is.null(graph$selection$nodes)){
    graph$selection$nodes <- NULL
  }

  return(graph)
}
