#' Add edges to an existing graph object
#' With a graph object of class 'gv_graph', add one or more edges of specified types to nodes within the graph.
#' @param graph a graph object of class 'gv_graph' that is created using 'create_graph'.
#' @param edges_df an edge data frame that is created using 'create_edges'.
#' @param from a vector of the outgoing nodes from which each edge is connected.
#' @param to a vector of the incoming nodes to which each edge is connected.
#' @param relationship a string specifying the relationship between the connected nodes.
#' @return a graph object of class 'gv_graph'.
#' @export add_edges

add_edges <- function(graph,
                      edges_df = NULL,
                      from = NULL,
                      to = NULL,
                      relationship = NULL){

  edges_df_available <- FALSE
  from_to_available <- FALSE

  if (!is.null(edges_df)){

    # Ensure that the appropriate columns specifying edges are present
    edge_from_present <- "edge_from" %in% colnames(edges_df)
    edge_to_present <- "edge_to" %in% colnames(edges_df)

    from_present <- "from" %in% colnames(edges_df)
    to_present <- "to" %in% colnames(edges_df)

    edges_df_valid <- ((edge_from_present & edge_to_present) |
                         (from_present & to_present))

    # Ensure that the nodes specified are in the graph object
    all_nodes_in_graph <- all(get_nodes(edges_df) %in% get_nodes(graph))

    edges_df_available <- ifelse(edges_df_valid & all_nodes_in_graph,
                                 TRUE, FALSE)
  }

  if ((!is.null(edges_df) & edges_df_available == FALSE) |
      (!is.null(from) & !is.null(to))){

    # Ensure that the nodes specified are in the graph object
    all_nodes_in_graph <- all(unique(c(from, to)) %in% get_nodes(graph))

    from_to_available <- ifelse(all_nodes_in_graph, TRUE, FALSE)
  }

  # Create the new graph object
  if (is.null(graph$edges_df)){

    if (edges_df_available == TRUE){

      gv_graph <-
        create_graph(nodes_df = graph$nodes_df,
                       edges_df = edges_df)

      return(gv_graph)
    }

    if (from_to_available == TRUE){

      if (!is.null(relationship)){

        gv_graph <-
          create_graph(nodes_df = graph$nodes_df,
                         edges_df = create_edges(edge_from = from,
                                                 edge_to = to,
                                                 relationship = relationship))
      }

      if (is.null(relationship)){

        gv_graph <-
          create_graph(nodes_df = graph$nodes_df,
                         edges_df = create_edges(edge_from = from,
                                                 edge_to = to))
      }

      return(gv_graph)
    }
  }

  if (!is.null(graph$edges_df)){

    if (edges_df_available == TRUE){

      combined_edges <- combine_edges(graph$edges_df,
                                      edges_df)

      gv_graph <-
        create_graph(nodes_df = graph$nodes_df,
                       edges_df = combined_edges)

      return(gv_graph)
    }

    if (from_to_available == TRUE){

      if (!is.null(relationship)){

        combined_edges <- combine_edges(graph$edges_df,
                                      create_edges(edge_from = from,
                                                   edge_to = to,
                                                   relationship = relationship))
      }

      if (is.null(relationship)){

        combined_edges <- combine_edges(graph$edges_df,
                                        create_edges(edge_from = from,
                                                     edge_to = to))
      }

      gv_graph <-
        create_graph(nodes_df = graph$nodes_df,
                       edges_df = combined_edges)

      return(gv_graph)
    }
  }
}
