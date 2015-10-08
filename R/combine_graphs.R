#' Combine two graphs into a single graph
#' @description Combine two graphs in order to make a new graph, merging nodes
#' and edges in the process.
#' @param x a \code{DiagrammeR} graph object to which another graph will be
#' joined. This graph should be considered the host graph as the resulting
#' graph will retain only the attributes of this graph.
#' @param y a \code{DiagrammeR} graph object that is to be joined with the graph
#' suppled as \code{x}.
#' @return a graph object of class \code{dgr_graph}.
#' @export combine_graphs

combine_graphs <- function(x, y){

  if (is.null(x$dot_code)){

    dgr_graph <-
      create_graph(nodes_df = combine_nodes(x$nodes, y$nodes),
                   edges_df = combine_edges(x$edges, y$edges),
                   graph_attrs = x$graph_attrs,
                   node_attrs = x$node_attrs,
                   edge_attrs = x$edge_attrs,
                   directed = ifelse(is_graph_directed(x) == FALSE ||
                                       is_graph_directed(y) == FALSE,
                                     FALSE, TRUE),
                   graph_name = x$graph_name,
                   graph_time = x$graph_time,
                   graph_tz = x$graph_tz,
                   generate_dot = FALSE)
  }

  if (!is.null(x$dot_code)){

    dgr_graph <-
      create_graph(nodes_df = combine_nodes(x$nodes, y$nodes),
                   edges_df = combine_edges(x$edges, y$edges),
                   graph_attrs = x$graph_attrs,
                   node_attrs = x$node_attrs,
                   edge_attrs = x$edge_attrs,
                   directed = ifelse(is_graph_directed(x) == FALSE ||
                                       is_graph_directed(y) == FALSE,
                                     FALSE, TRUE),
                   graph_name = x$graph_name,
                   graph_time = x$graph_time,
                   graph_tz = x$graph_tz)
  }

  return(dgr_graph)
}
