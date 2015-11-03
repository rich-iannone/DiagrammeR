#' Get edge attributes based on a selection of edges
#' @description From a graph object of class \code{dgr_graph}, get edge
#' attribute properties for edges available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export get_edge_attr_from_selection

get_edge_attr_from_selection <- function(graph){

  if (is.null(graph$selection$edges)){
    stop("There is no selection of nodes available.")
  }

  edges_df <- get_edge_attr(graph,
                            from = graph$selection$edges$from,
                            to = graph$selection$edges$to)

  return(edges_df)
}
