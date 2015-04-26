#' Get vector of node IDs
#'
#' Provides information on the node IDs from one or several node data frames, edge data frames, or graph objects.
#'
#' @param ... a collection of node data frames, edge data frames, or graph objects.
#' @export get_nodes

get_nodes <- function(...){

  objects <- list(...)

  for (i in 1:length(objects)){

    if (i == 1) node_ID <- vector(mode = "character")

    }

  }

  all_ID_unique <- ifelse(anyDuplicated(node_ID) == 0, TRUE, FALSE)

  if (all_ID_unique == TRUE){
    return(node_ID)
  }
}
