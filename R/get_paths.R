#' Get paths from a specified node in a directed graph
#' @description Obtain a list of all possible paths from a given node within
#' a directed graph
#' @param graph a graph object of class \code{dgr_graph}.
#' @param node the node from which all paths will be determined.
#' @return a list of paths comprising vectors of node IDs in sequence of
#' traversal through the graph
#' @export get_paths

get_paths <- function(graph,
                      node){

  if (all(is.na(get_successors(graph, node)))){
    return(NA)
  }

  # Initialize paths with starting node
  paths <- list(node)

  repeat{

    for (i in 1:length(paths)){

      if (any(!is.na(get_successors(graph,
                                    paths[[i]][length(paths[[i]])])))){

        # Get the successors for the last node in the given path
        next_nodes <- get_successors(graph, paths[[i]][length(paths[[i]])])

        # Filter next_nodes if cycles are detected
        next_nodes <- next_nodes[which(!(next_nodes %in%
          paths[[i]][1:length(paths[[i]]) - 1]))]

        if (length(next_nodes) > 1){

          for (j in 1:length(next_nodes)){

            if (j == 1) paths[[i]] <-
                c(paths[[i]], next_nodes[1])

            if (j > 1) paths[[length(paths) + 1]] <-
                c(paths[[i]][-length(paths[[i]])], next_nodes[j])
          }
        }

        if (length(next_nodes) == 1){

          paths[[i]] <- c(paths[[i]], next_nodes[1])
        }
      }
    }

    for (k in 1:length(paths)){

      if (k == 1) check <- vector()

      check <- c(check, any(is.na(get_successors(graph,
                                                 paths[[k]][length(paths[[k]])]))|
                              (paths[[k]][length(paths[[k]])] %in%
                                 paths[[k]][1:length(paths[[k]]) - 1])))

      if (paths[[k]][length(paths[[k]])] %in%
          paths[[k]][1:length(paths[[k]]) - 1]){
        paths[[k]] <- paths[[k]][-length(paths[[k]])]
      }
    }

    if (all(check)) break
  }

  # Arrange vectors in list in order of increasing length
  order <- sapply(1:length(paths), function(x) length(paths[[x]]))
  names(order) <- 1:length(paths)
  order <- sort(order)
  order <- as.numeric(names(order))
  paths <- paths[order]

  return(paths)

}
