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

  paths <- list(node)

  repeat{

    for (i in 1:length(paths)){

      if (any(!is.na(get_successors(graph,
                                    paths[[i]][length(paths[[i]])])))){

        next_nodes <- get_successors(graph, paths[[i]][length(paths[[i]])])

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

      check <- c(check, is.na(get_successors(graph,
                                             paths[[k]][length(paths[[k]])])))
    }

    if (all(check)) break
  }

  return(paths)
}
