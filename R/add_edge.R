#' Add an edge between nodes in a graph object
#' @description With a graph object of class \code{dgr_graph}, add an edge
#' to nodes within the graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param from the outgoing node from which the edge is connected.
#' @param to the incoming nodes to which each edge is connected.
#' @param rel an optional string specifying the relationship between the
#' connected nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a graph with two nodes
#' graph <- create_graph(create_nodes(nodes = c("a", "b")))
#'
#' # Add an edge between those nodes and attach a relationship to the edge
#' graph <- add_edge(graph, from = "a", to = "b", rel = "to_get")
#' }
#' @export add_edge

add_edge <- function(graph,
                     from,
                     to,
                     rel = NULL){

  if (is_graph_empty(graph) == TRUE){
    stop("Edges cannot be added to an empty graph.")
  }

  if (length(from) > 1 | length(to) > 1){
    stop("Only one edge can be specified.")
  }

  # If an edge between nodes is requested and that edge exists, stop function
  if (all(!is.na(get_edges(graph, return_type = "vector")))){

    if (any(get_edges(graph)[[1]] == from &
            get_edges(graph)[[2]] == to)){
      stop("This edge already exists.")
    }
  }

  # If 'graph$edges_df' is null then use 'create_edges' to add an edge
  if (is.null(graph$edges_df)){

    # If a relationship is defined, add that in the 'create_edges' call
    if (!is.null(rel)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = create_edges(from = from,
                                             to = to,
                                             rel = rel),
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)
    }

    # If a relationship is not defined, use a simpler 'create_edges' call
    if (is.null(rel)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = create_edges(from = from,
                                             to = to),
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      return(dgr_graph)
    }
  }

  # If 'graph$edges_df' is not null then use both 'combine_edges' and
  # 'create_edges' to add an edge
  if (!is.null(graph$edges_df)){

    # If a relationship is defined, add that in the 'create_edges' call
    if (!is.null(rel)){

      combined_edges <-
        combine_edges(graph$edges_df,
                      create_edges(from = from,
                                   to = to,
                                   rel = rel))
    }

    # If a relationship is not defined, use a simpler 'create_edges' call
    if (is.null(rel)){

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
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }

  return(dgr_graph)
}
