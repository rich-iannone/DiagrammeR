#' Get an edge data frame from a graph
#' @description From a graph, obtain an edge data frame with all current
#' edge attributes.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return an edge data frame.
#' @export get_edge_df

get_edge_df <- function(graph){

  if (is.null(graph$edges_df)){
    return(NA)
  } else{
    return(graph$edges_df)
  }
}
