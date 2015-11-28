#' Combine two graphs into a single graph
#' @description Combine two graphs in order to make a new graph, merging nodes
#' and edges in the process. The use of an optional data frame allows for
#' new edges to be formed across the combined graphs.
#' @param x a \code{DiagrammeR} graph object to which another graph will be
#' joined. This graph should be considered the host graph as the resulting
#' graph will retain only the attributes of this graph.
#' @param y a \code{DiagrammeR} graph object that is to be joined with the graph
#' suppled as \code{x}.
#' @param edges_df an optional edge data frame that allows for connections
#' between nodes across the graphs to be combined.
#' @return a graph object of class \code{dgr_graph}.
#' @export combine_graphs

combine_graphs <- function(x, y,
                           edges_df = NULL){

  if (any(get_nodes(x) %in% get_nodes(y))){
    stop("Cannot combine graphs with common node ID values")
  }

  combined_nodes <- combine_nodes(x$nodes_df,
                                  y$nodes_df)

  if (!is.null(edges_df)){

    # Stop function if edge data frame doesn't fulfill certain conditions
    if (!(all(unique(c(edges_df$from, edges_df$to)) %in%
              c(get_nodes(x), get_nodes(y))))){
      stop("Not all nodes in this edge data frame exist in the 2 graphs.")
    }

    for (i in 1:nrow(edges_df)){

      if (edges_df$from[i] %in% get_nodes(x)){
        if ((edges_df$to[i] %in% get_nodes(y)) == FALSE){
          stop("Edges supplied in this edge data frame must be across graphs.")
        }
      }

      if (edges_df$from[i] %in% get_nodes(y)){
        if ((edges_df$to[i] %in% get_nodes(x)) == FALSE){
          stop("Edges supplied in this edge data frame must be across graphs.")
        }
      }

      if (edges_df$from[i] == edges_df$to[i]){
        stop("Edges supplied in this edge data frame cannot contain loops.")
      }
    }

    combined_edges <- combine_edges(x$edges_df,
                                    y$edges_df,
                                    edges_df)
  }

  if (is.null(edges_df)){
    combined_edges <- combine_edges(x$edges_df,
                                    y$edges_df)
  }

  if (is.null(x$dot_code)){

    dgr_graph <-
      create_graph(nodes_df = combined_nodes,
                   edges_df = combined_edges,
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
      create_graph(nodes_df = combined_nodes,
                   edges_df = combined_edges,
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
