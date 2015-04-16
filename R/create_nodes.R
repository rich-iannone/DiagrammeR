#' Create nodes for Graphviz graphs
#' @description Combine several named vectors for nodes and their attributes.
#' @param ... one or more named vectors for nodes and associated attributes
#' @return a node data frame
#' @export

create_nodes <- function(...){

  nodes <- list(...)

  number_of_nodes <- length(nodes$nodes)
  # Stop function if there are no list components
  stopifnot(!is.null(names(nodes)))


  for (i in 1:length(nodes)){

    # Expand vectors with single values to fill to number of nodes
    if (length(nodes[[i]]) == 1){
      nodes[[i]] <- rep(nodes[[i]], number_of_nodes)
    }

    # Expand vectors with length > 1 and length < 'number_of_nodes'
    if (length(nodes[[i]]) > 1 & length(nodes[[i]]) < number_of_nodes){
      nodes[[i]] <- c(nodes[[i]], rep("", (number_of_nodes - length(nodes[[i]]))))
    }

    # Trim vectors with number of values exceeding number of nodes
    if (length(nodes[[i]]) > number_of_nodes){
      nodes[[i]] <- nodes[[i]][1:number_of_nodes]
    }
  }

  nodes_df <- as.data.frame(nodes)

  return(nodes_df)
}
