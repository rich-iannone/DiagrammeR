#' Traverse inward to a selected node, skipping over edges, and creating
#' a new node selection
#' @description From a graph object of class \code{dgr_graph} move outward
#' from one or more nodes present in a selection to other nodes, replacing
#' the current nodes in the selection with those nodes traversed to.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export trav_in

trav_in <- function(graph){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  if (is.null(graph$traversals)){
    return(graph)
  }

  current_selection <- graph$selection$nodes

  selection_node <- graph$traversals[[length(graph$traversals)]][1]

  graph$selection$nodes <- selection_node

  for (i in length(graph$traversals):1){
    if (graph$traversals[[i]][2] %in% current_selection){
      graph$traversals[[i]] <- NULL
    }
  }

  if (!is.null(graph$traversals)){

    if (length(graph$traversals) == 0){
      graph$traversals <- NULL
    }
  }

  return(graph)
}
