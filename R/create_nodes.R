create_nodes <- function(...){

  nodes <- list(...)

  number_of_nodes <- length(nodes$nodes)

  for (i in 1:length(nodes)){

    # Expand vectors with single values to fill to number of nodes
    if (length(nodes[[i]]) == 1){
      nodes[[i]] <- rep(nodes[[i]], number_of_nodes)
    }

    # Trim vectors with number of values exceeding number of nodes
    if (length(nodes[[i]]) > number_of_nodes){
      nodes[[i]] <- nodes[[i]][1:number_of_nodes]
    }
  }

  nodes_df <- as.data.frame(nodes)

}
