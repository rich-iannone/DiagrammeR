#' Create, read, update, delete, or report status of a node type definition
#' @description From a graph object of class 'gv_graph', query a node in the graph (using the node ID) and perform operations on the type definition for that node.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a node ID corresponding to the node to be selected.
#' @param mode the type of operation to perform post-query. To remove the type definition from a node, use either 'delete', 'remove', or 'drop'. To add a type definition to a node with no type set, use 'add' or 'create'. To update a node's type definition, use 'update'. To return the value of a node type, use 'read'. To determine whether there is a type set for the selected node, use 'available'.
#' @param value a string denoting the node type, supplied only if 'mode' was set to either 'add', 'create', or 'update'.
#' @return a graph object of class 'gv_graph'.
#' @export node_type

node_type <- function(graph,
                      node,
                      mode = "read",
                      value = NULL){


  # Determine if node is present within the graph
  node_is_in_graph <- node_present(graph = graph, node = node)

  # Stop function if node is not present within the graph
  if (node_is_in_graph == FALSE){

    stop("The specified node is not present in the graph.")
  }

  if (node_is_in_graph == TRUE){

    node_row <- which(graph$nodes_df$nodes == node)

    type_set <- ifelse(graph$nodes_df$type[node_row] == "",
                               FALSE, TRUE)

    # Remove type if a type is set
    if (mode %in% c("delete", "remove", "drop")){

      if (type_set == FALSE){

        return(graph)
      }

      if (type_set == TRUE){

        graph$nodes_df$type[node_row] <- ""

        return(graph)
      }
    }

    # Add a type to a node with no type definition set
    if (mode %in% c("add", "create")){

      if (type_set == TRUE){

        return(graph)
      }

      if (type_set == FALSE & !is.null(value)){

        graph$nodes_df$type[node_row] <- value

        return(graph)
      }
    }

    # Update an existing type definition for a node
    if (mode == "update"){

      if (type_set == FALSE){

        return(graph)
      }

      if (type_set == TRUE & !is.null(value)){

        graph$nodes_df$type[node_row] <- value

        return(graph)
      }
    }

    # Return the value of an existing node type
    if (mode == "read"){

      if (type_set == FALSE){

        return(NA)
      }

      if (type_set == TRUE){

        type_value <- graph$nodes_df$type[node_row]

        return(type_value)
      }
    }
