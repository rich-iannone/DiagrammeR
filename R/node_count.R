#' Get count of all nodes or certain types of nodes
#' @description From a graph object of class 'gv_graph', get a count of nodes in the graph and optionally obtain count of nodes by their type.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a value that may or may not match a node ID in the graph.
#' @param type either a logical value, where TRUE provides a named vector of node count by type and FALSE (the default) provides a total count, or, a string corresponding to one or more node types.
#' @return a logical value
#' @export node_count

node_count <- function(graph,
                       type = FALSE){

  # If type is FALSE, get a total count of nodes
  if (type == FALSE){

    if (is_graph_empty(graph) == TRUE){

      total_node_count <- 0
    }

    if (is_graph_empty(graph) == FALSE){

      total_node_count <- length(get_nodes(graph))
    }

    return(total_node_count)
  }

  # If type set to TRUE, get a named vector of counts by type
  if (type == TRUE){

    if (is_graph_empty(graph) == TRUE){

      total_node_count <- 0
    }

    if (is_graph_empty(graph) == FALSE){

      for (i in 1:length(get_nodes(graph))){

        if (i == 1){
          all_nodes <- get_nodes(graph)
          all_types <- vector(mode = "character")
        }

        all_types <- c(all_types,
                       node_type(graph = graph,
                                 all_nodes[i],
                                 mode = "read"))
        all_types <- unique(all_types)

        if (any(is.na(all_types))){

          all_types[which(is.na(all_types))] <- ""
        }
      }

      for (i in 1:length(all_types)){

        if (i == 1) total_node_count <- vector(mode = "numeric")

        total_node_count <-
          c(total_node_count,
            nrow(graph$nodes_df[which(graph$nodes_df$type == all_types[i]),]))


        if (i == length(all_types)){
          names(total_node_count) <- all_types
        }
      }

      return(total_node_count)
    }

    # If type is a character vector, get counts by supplied types
    if (class(type) == "character"){

      if (is_graph_empty(graph) == TRUE){

        total_node_count <- 0
      }

      if (is_graph_empty(graph) == FALSE){

        for (i in 1:length(get_nodes(graph))){

          if (i == 1){
            all_nodes <- get_nodes(graph)
            all_types <- vector(mode = "character")
          }

          all_types <- c(all_types,
                         node_type(graph = graph,
                                   all_nodes[i],
                                   mode = "read"))
          all_types <- unique(all_types)

        }

        types_are_available <- ifelse(all(type %in% all_types), TRUE, FALSE)

        if (types_are_available){

          for (i in 1:length(type)){

            if (i == 1) total_node_count <- vector(mode = "numeric")

            total_node_count <-
              c(total_node_count,
                nrow(nodes_df_set_type[which(nodes_df_set_type$type == type[i]),]))

            if (i == length(type)){

              names(total_node_count) <- type
            }
          }
        }

          if (types_are_available == FALSE){

            total_node_count <- NA
          }

          return(total_node_count)
        }
      }
    }
  }
}
