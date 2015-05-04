#' Add a node to an existing graph object
#' @description With a graph object of class 'gv_graph', add a new node of a specified type to extant nodes within the graph.
#' @param graph a graph object of class 'gv_graph' that is created using 'graphviz_graph'.
#' @param node a node ID for the newly connected node.
#' @param from an optional vector containing node IDs from which edges will be directed to the new node.
#' @param to an optional vector containing node IDs to which edges will be directed from the new node.
#' @param label a character object for supplying an optional label to the node. Setting to TRUE ascribes the node ID to the label. Setting to FALSE yields a blank label.
#' @param type an optional string that describes the entity type for the node to be added.
#' @return a graph object of class 'gv_graph'.
#' @export add_node

add_node <- function(graph,
                     node,
                     from = NULL,
                     to = NULL,
                     label = TRUE,
                     type = NULL){

  # Verify that 'node' is given as a single value
  node_is_single_value <- ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE){

    stop("Only a single node can be added using 'add_node")
  }

  # Determine whether node to add is already in graph
  if (node_is_single_value == TRUE){

    can_add_node_id <- ifelse(!(node %in% get_nodes(graph)), TRUE, FALSE)
  }

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

      combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(from = from,
                                                   to = rep(node, length(from))))

      gv_graph <-
        graphviz_graph(nodes_df = combined_nodes,
                       edges_df = combined_edges)

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

      combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(from = rep(node, length(to)),
                                                   to = to))

      gv_graph <-
        graphviz_graph(nodes_df = combined_nodes,
                       edges_df = combined_edges)

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

      combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(from = from,
                                                   to = rep(node, length(from))))

      combined_edges <- combine_edges(combined_edges,
                                      create_edges(from = rep(node, length(to)),
                                                   to = to))

      gv_graph <-
        graphviz_graph(nodes_df = combined_nodes,
                       edges_df = combined_edges)

      return(gv_graph)
    }
  }

  # Modify graph if none of 'to' nor 'from' values provided
  if (is.null(from) & is.null(to)){

      combined_nodes <- combine_nodes(graph$nodes_df,
                                      create_nodes(nodes = node,
                                                   label = label,
                                                   type = type))

      gv_graph <-
        graphviz_graph(nodes_df = combined_nodes,
                       edges_df = graph$edges_df)

      return(gv_graph)
    }
  }



}
