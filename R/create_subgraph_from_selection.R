#' Create a subgraph based on a selection of nodes or edges
#' @description Create a subgraph based on a selection of nodes or edges
#' extant in the graph object.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export create_subgraph_from_selection

create_subgraph_from_selection <- function(graph){

  # Stop function if the graph does not contain a selection
  if (is.null(graph$selection)){
    stop("The graph does not contain an active selection")
  }

  # Filter the nodes in the graph
  if (!is.null(graph$selection$nodes)){

    selection_nodes <- graph$selection$nodes

    selection_nodes_df <-
      graph$nodes_df[which(graph$nodes_df$nodes %in% selection_nodes),]

    selection_edges_df <-
      graph$edges_df[which(graph$edges_df$from %in% selection_nodes &
                             graph$edges_df$to %in% selection_nodes),]
  }

  # Filter the edges in the graph
  if (!is.null(graph$selection$edges)){

    selection_from <- graph$selection$edges$from
    selection_to <- graph$selection$edges$to

    selection_edges_df <-
      graph$edges_df[which(graph$edges_df$from %in% selection_from &
                             graph$edges_df$to %in% selection_to),]

    selection_nodes_df <-
      graph$nodes_df[which(graph$nodes_df$nodes %in%
                             unique(c(selection_edges_df$from,
                                      selection_edges_df$to))),]
  }

  # Create a subgraph
  subgraph <-
    create_graph(nodes_df = selection_nodes_df,
                 edges_df = selection_edges_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  # Return the subgraph
  return(subgraph)
}
