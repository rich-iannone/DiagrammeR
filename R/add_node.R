#' Add a node to an existing graph object
#' With a graph object of class 'gv_graph', add a new node of a specified type to extant nodes within the graph.
#' @param graph a graph object of class 'gv_graph' that is created using 'create_graph'.
#' @param node a node ID for the newly connected node.
#' @param from an optional vector containing node IDs from which edges will be directed to the new node.
#' @param to an optional vector containing node IDs to which edges will be directed from the new node.
#' @param label a character object for supplying an optional label to the node. Setting to TRUE ascribes the node ID to the label. Setting to FALSE yields a blank label.
#' @param type an optional string that describes the entity type for the node to be added.
#' @param ... one or more vectors pertaining to node attributes.
#' @return a graph object of class 'gv_graph'.
#' @export add_node

add_node <- function(graph,
                     node,
                     from = NULL,
                     to = NULL,
                     label = TRUE,
                     type = NULL,
                     ...){

  # Verify that 'node' is given as a single value
  node_is_single_value <- ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE){

    stop("Only a single node can be added using 'add_node'.")
  }

  # Determine whether node to add is already in graph
  if (node_is_single_value == TRUE){

    can_add_node_id <- ifelse(!node_present(graph = graph, node = node), TRUE, FALSE)
  }

  if (can_add_node_id == FALSE){
    stop("The node is already present in the graph.")
  }

  # Place triple dot vectors in a list
  node_attributes <- list(...)

  # Determine whether optional node attributes provided
  node_attributes_provided <- ifelse(length(node_attributes) > 0, TRUE, FALSE)

  # Modify graph if only 'from' values provided
  if (!is.null(from) & is.null(to)){

    from_nodes_available <- ifelse(all(from %in% get_nodes(graph)), TRUE, FALSE)

    if (from_nodes_available == FALSE){
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (from_nodes_available){

      combined_nodes <- combine_nodes(graph$nodes_df,
                                      create_nodes(nodes = node,
                                                   label = label,
                                                   type = type))

      if (node_attributes_provided == TRUE){

        for (i in 1:length(node_attributes)){

          if (names(node_attributes)[i] %in% colnames(combined_nodes)){

            combined_nodes[nrow(combined_nodes),
                           which(colnames(combined_nodes) == names(node_attributes)[i])] <-
              node_attributes[i][[1]]
          }

          if (!(names(node_attributes)[i] %in% colnames(combined_nodes))){

            combined_nodes <- cbind(combined_nodes,
                                    c(rep("", length = nrow(combined_nodes) - 1),
                                      node_attributes[i][[1]]))

            colnames(combined_nodes)[ncol(combined_nodes)] <- names(node_attributes)[i]
          }
        }
      }

      combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(edge_from = from,
                                                   edge_to = rep(node, length(from))))

      gv_graph <-
        create_graph(nodes_df = combined_nodes,
                     edges_df = combined_edges)

      # Create a revised graph and return that graph
      return(gv_graph)
    }
  }

  # Modify graph if only 'to' values provided
  if (is.null(from) & !is.null(to)){

    to_nodes_available <- ifelse(all(to %in% get_nodes(graph)), TRUE, FALSE)

    if (to_nodes_available == FALSE){
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    if (from_nodes_available){

      combined_nodes <- combine_nodes(graph$nodes_df,
                                      create_nodes(nodes = node,
                                                   label = label,
                                                   type = type))

      if (node_attributes_provided == TRUE){

        for (i in 1:length(node_attributes)){

          if (names(node_attributes)[i] %in% colnames(combined_nodes)){

            combined_nodes[nrow(combined_nodes),
                           which(colnames(combined_nodes) == names(node_attributes)[i])] <-
              node_attributes[i][[1]]
          }

          if (!(names(node_attributes)[i] %in% colnames(combined_nodes))){

            combined_nodes <- cbind(combined_nodes,
                                    c(rep("", length = nrow(combined_nodes) - 1),
                                      node_attributes[i][[1]]))

            colnames(combined_nodes)[ncol(combined_nodes)] <- names(node_attributes)[i]
          }
        }
      }

      combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(edge_from = rep(node, length(to)),
                                                   edge_to = to))

      gv_graph <-
        create_graph(nodes_df = combined_nodes,
                     edges_df = combined_edges)

      # Create a revised graph and return that graph
      return(gv_graph)
    }
  }

  # Modify graph if both 'to' and 'from' values provided
  if (!is.null(from) & !is.null(to)){

    from_nodes_available <- ifelse(all(from %in% get_nodes(graph)), TRUE, FALSE)

    to_nodes_available <- ifelse(all(to %in% get_nodes(graph)), TRUE, FALSE)

    if (from_nodes_available == FALSE){
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (to_nodes_available == FALSE){
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    if (from_nodes_available & to_nodes_available){

      combined_nodes <- combine_nodes(graph$nodes_df,
                                      create_nodes(nodes = node,
                                                   label = label,
                                                   type = type))

      if (node_attributes_provided == TRUE){

        for (i in 1:length(node_attributes)){

          if (names(node_attributes)[i] %in% colnames(combined_nodes)){

            combined_nodes[nrow(combined_nodes),
                           which(colnames(combined_nodes) == names(node_attributes)[i])] <-
              node_attributes[i][[1]]
          }

          if (!(names(node_attributes)[i] %in% colnames(combined_nodes))){

            combined_nodes <- cbind(combined_nodes,
                                    c(rep("", length = nrow(combined_nodes) - 1),
                                      node_attributes[i][[1]]))

            colnames(combined_nodes)[ncol(combined_nodes)] <- names(node_attributes)[i]
          }
        }
      }

      combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(edge_from = from,
                                                   edge_to = rep(node, length(from))))

      combined_edges <- combine_edges(combined_edges,
                                      create_edges(edge_from = rep(node, length(to)),
                                                   edge_to = to))

      gv_graph <-
        create_graph(nodes_df = combined_nodes,
                     edges_df = combined_edges)

      # Create a revised graph and return that graph
      return(gv_graph)
    }
  }

  # Modify graph if none of 'to' nor 'from' values provided
  if (is.null(from) & is.null(to)){

    combined_nodes <- combine_nodes(graph$nodes_df,
                                    create_nodes(nodes = node,
                                                 label = label,
                                                 type = type))

    if (node_attributes_provided == TRUE){

      for (i in 1:length(node_attributes)){

        if (names(node_attributes)[i] %in% colnames(combined_nodes)){

          combined_nodes[nrow(combined_nodes),
                         which(colnames(combined_nodes) == names(node_attributes)[i])] <-
            node_attributes[i][[1]]
        }

        if (!(names(node_attributes)[i] %in% colnames(combined_nodes))){

          combined_nodes <- cbind(combined_nodes,
                c(rep("", length = nrow(combined_nodes) - 1),
                  node_attributes[i][[1]]))

          colnames(combined_nodes)[ncol(combined_nodes)] <- names(node_attributes)[i]
        }
      }

      # Create a revised graph and return that graph
      gv_graph <-
        create_graph(nodes_df = combined_nodes,
                       edges_df = graph$edges_df)

      return(gv_graph)
    }
  }
}
