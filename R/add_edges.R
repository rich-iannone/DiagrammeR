#' Add edges to an existing graph object
#' @description With a graph object of class \code{dgr_graph}, add one or more
#' edges of specified types to nodes within the graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edges_df an edge data frame that is created using \code{create_edges}.
#' @param from a vector of the outgoing nodes from which each edge is connected.
#' @param to a vector of the incoming nodes to which each edge is connected.
#' @param relationship a string specifying the relationship between the
#' connected nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a graph with two nodes
#' graph <- create_graph(create_nodes(nodes = c("a", "b")))
#'
#' # Add an edge between those nodes and attach a relationship
#' graph <- add_edges(graph, from = "a", to = "b",
#'                    relationship = "to_get")
#' }
#' @export add_edges

add_edges <- function(graph,
                      edges_df = NULL,
                      from = NULL,
                      to = NULL,
                      relationship = NULL){

  if (is_graph_empty(graph) == TRUE){

    message("Edges cannot be added to an empty graph")
    return(graph)
  }

  # If an edge data frame is supplied, it will be used to add edges to
  # the graph's nodes
  if (!is.null(edges_df)){

    # Ensure that the appropriate columns specifying edges are present
    from_present <- "from" %in% colnames(edges_df)
    to_present <- "to" %in% colnames(edges_df)

    edges_df_valid <- from_present & to_present

    # Ensure that the nodes specified are in the graph object
    all_nodes_in_graph <- all(get_nodes(edges_df) %in% get_nodes(graph))

    edges_df_available <- ifelse(edges_df_valid & all_nodes_in_graph,
                                 TRUE, FALSE)

    # If not all the nodes specified in the edge data frame are in the
    # graph, return the original graph with a message
    if (edges_df_available == FALSE){

      message("Not all nodes specified in the edge data frame are in the graph")
      return(graph)
    }

    # If the 'edges_df' component of the graph is not null, combine the
    # incoming edge data frame with the existing edge definitions in the
    # graph object
    if (!is.null(graph$edges_df)){

      combined_edges <- combine_edges(graph$edges_df,
                                      edges_df)

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = combined_edges,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      return(dgr_graph)
    }

    # If the 'edges_df' component of the graph is null, insert the
    # edge data frame into the graph object
    if (is.null(graph$edges_df)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = edges_df,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      return(dgr_graph)
    }
  }

  # If an edge between nodes is requested and that edge exists, return
  # the initial graph with a message
  if (!is.null(from) & !is.null(to) &
      !is.na(get_edges(graph, return_type = "vector"))){

    if (any(get_edges(graph)[[1]] == from &
            get_edges(graph)[[2]] == to)){

      message("This edge already exists")
      return(graph)
    }
  }

  # If 'graph$edges_df' is null then use 'create_edges' to add an edge
  if (is.null(graph$edges_df)){

    # If a relationship is defined, add that in the 'create_edges' call
    if (!is.null(relationship)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = create_edges(from = from,
                                             to = to,
                                             relationship = relationship),
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)
    }

    # If a relationship is not defined, use a simpler 'create_edges' call
    if (is.null(relationship)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = create_edges(from = from,
                                             to = to),
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)
    }

    return(dgr_graph)
  }



  # If 'graph$edges_df' is not null then use both 'combine_edges' and
  # 'create_edges' to add an edge
  if (!is.null(graph$edges_df)){

    # If a relationship is defined, add that in the 'create_edges' call
    if (!is.null(relationship)){

      combined_edges <-
        combine_edges(graph$edges_df,
                      create_edges(from = from,
                                   to = to,
                                   relationship = relationship))
    }

    # If a relationship is not defined, use a simpler 'create_edges' call
    if (is.null(relationship)){

      combined_edges <-
        combine_edges(graph$edges_df,
                      create_edges(from = from,
                                   to = to))
    }

    # Use the 'combined_edges' object in either case to create an updated graph
    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = combined_edges,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }
}
