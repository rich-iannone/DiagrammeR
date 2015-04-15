
create_edges <- function(...){

  edges <- list(...)

  number_of_edges <- length(edges$edge_from)

  for (i in 1:length(edges)){

    # Expand vectors with single values to fill to number of edges
    if (length(edges[[i]]) == 1){
      edges[[i]] <- rep(edges[[i]], number_of_edges)
    }

    # Expand vectors with length > 1 and length < 'number_of_edges'
    if (length(edges[[i]]) > 1 & length(edges[[i]]) < number_of_edges){
      edges[[i]] <- c(edges[[i]], rep("", (number_of_edges - length(edges[[i]]))))
    }

    # Trim vectors with number of values exceeding number of edges
    if (length(edges[[i]]) > number_of_edges){
      edges[[i]] <- edges[[i]][1:number_of_edges]
    }
  }

  edges_df <- as.data.frame(edges)

  return(edges_df)
}
