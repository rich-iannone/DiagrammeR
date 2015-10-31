#' Get a node data frame from a graph
#' @description From a graph, obtain a node data frame with all current
#' node attributes.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a node data frame.
#' @export get_node_df

get_node_df <- function(graph){

  if (is.null(graph$nodes_df)){
    return(NA)
  } else{
    return(graph$nodes_df)
  }
}
