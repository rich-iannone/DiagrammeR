#' Create a data frame with nodes and their attributes
#' Combine several named vectors for nodes and their attributes into a data frame, which can be combined with other similarly-generated data frame, or, added to a graph object.
#' @param ... one or more named vectors for nodes and associated attributes.
#' @return a data frame.
#' @export create_nodes

create_nodes <- function(...){

  nodes <- list(...)

  # Stop function if there are no named list components
  stopifnot(!is.null(names(nodes)))

  # Attempt to obtain the number of nodes from the 'node' column
  if ("node" %in% names(nodes)){
    nodes_column <- which("node" %in% names(nodes))
    number_of_nodes <- length(nodes[nodes_column][[1]])
  }

  # Attempt to obtain the number of nodes from the 'nodes' column
  if ("nodes" %in% names(nodes)){
    nodes_column <- which("nodes" %in% names(nodes))
    number_of_nodes <- length(nodes[nodes_column][[1]])
  }

  # Attempt to obtain the number of nodes from the 'node_id' column
  if ("node_id" %in% names(nodes)){
    nodes_column <- which("node_id" %in% names(nodes))
    number_of_nodes <- length(nodes[nodes_column][[1]])
  }

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

    # Change logical for labels to empty labels
    if (names(nodes)[i] == "label" & class(nodes[[i]]) == "logical"){
      nodes[[i]] <- as.character(nodes[[i]])

      for (j in 1:length(nodes[[i]])){
        nodes[[i]][j] <- ifelse(nodes[[i]][j] == "FALSE", " ", nodes[nodes_column][[1]][j])
      }
    }
  }

  nodes_df <- as.data.frame(nodes, stringsAsFactors = FALSE)

  return(nodes_df)
}
