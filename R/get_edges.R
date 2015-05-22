#' Get node IDs associated with edges
#' Provides information on the node IDs associated with edges from one or more edge data frames, or, a graph object.
#' @param ... a collection of edge data frames or graph objects.
#' @param return_type using \code{list} (the default) will provide a list object containing vectors of outgoing and incoming node IDs associated with edges. With \code{df}, a data frame containing outgoing and incoming node IDs associated with edges. With \code{vector} or \code{string}, a vector of character objects representing the edges is provided.
#' @return a list, data frame, or a vector object, depending on the value given to \code{return_type}.
#' @export get_edges

get_edges <- function(...,
                      return_type = "list"){

  objects <- list(...)

  for (i in 1:length(objects)){

    if (i == 1) {
      edge_list <- vector(mode = "list")
      edge_list[[1]] <- edge_list[[2]] <- vector(mode = "character")
    }

    object <- objects[[i]]

    if (class(object) == "dgr_graph"){

      object_type <- "dgr_graph"
    }

    if (any(c("edge_from", "edge_to", "from", "to") %in% colnames(object))){

      object_type <- "edge_df"
    }
  }

  if (object_type == "dgr_graph"){

    object <- object$edges_df

    if ("edge_from" %in% colnames(object)){

      from_column <- which(colnames(object) == "edge_from")

    } else if ("from" %in% colnames(object)){

      from_column <- which(colnames(object) == "from")

    } else {

      stop("There is no column with edge information.")
    }

    if ("edge_to" %in% colnames(object)){

      to_column <- which(colnames(object) == "edge_to")

    } else if ("to" %in% colnames(object)){

      to_column <- which(colnames(object) == "to")

    } else {

      stop("There is no column with edge information.")
    }

    edge_list[[1]] <- c(edge_list[[1]], object[,from_column])
    edge_list[[2]] <- c(edge_list[[2]], object[,to_column])
  }

  if (object_type == "edge_df"){

    both_from_to_columns <- all(c(any(c("edge_from", "from") %in%
                                        colnames(object))),
                                any(c("edge_to", "to") %in%
                                      colnames(object)))

    if (exists("both_from_to_columns")){

      if (both_from_to_columns == TRUE){

        from_column <- which(colnames(object) %in% c("edge_from", "from"))[1]

        to_column <- which(colnames(object) %in% c("edge_to", "to"))[1]
      }
    }

    edge_list[[1]] <- c(edge_list[[1]], object[,from_column])
    edge_list[[2]] <- c(edge_list[[2]], object[,to_column])
  }

  if (return_type == "list"){

    return(edge_list)
  }

  if (return_type == "df"){

    edge_df <- as.data.frame(edge_list)
    colnames(edge_df) <- c("from", "to")

    return(edge_df)
  }

  if (return_type %in% c("vector", "string")){

    edge_vector <- paste(edge_list[[1]], "->", edge_list[[2]])

    return(edge_vector)
  }
}
