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

    object <- objects[[i]]

    if (class(object) == "gv_graph"){

      object_type <- "gv_graph"
    }

    if (class(object) == "data.frame"){

      if (any(c("nodes", "node", "node_ID") %in% colnames(object))){

        object_type <- "node_df"
      }

      if (any(c("edge_from", "edge_to", "from", "to") %in% colnames(object))){

        object_type <- "edge_df"
      }
    }
    }

  }

  all_ID_unique <- ifelse(anyDuplicated(node_ID) == 0, TRUE, FALSE)

  if (all_ID_unique == TRUE){
    return(node_ID)
  }
}
