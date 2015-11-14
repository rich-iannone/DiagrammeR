#' Delete all selected edges
#' @description In a graph object of class \code{dgr_graph}, delete
#' all edges present in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @export delete_edges_in_selection

delete_edges_in_selection <- function(graph){

  # If no edge selection is available, return the graph unchanged
  if (is.null(graph$selection$edges)){
    return(graph)
  }

  # Get vectors of the nodes in edges to be deleted
  from_delete <- graph$selection$edges$from
  to_delete <- graph$selection$edges$to

  # Delete all edges in selection
  for (i in 1:length(from_delete)){

    graph <-
      delete_edge(graph = graph,
                  from = from_delete[i],
                  to = to_delete[i])
  }

  # If the graph's edf has no rows, ensure that it set to null
  if (!is.null(graph$edges_df)){

    if (nrow(graph$edges_df) == 0){

      graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = NULL,
                     directed = graph$directed,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_tz = graph$graph_tz,
                     graph_time = graph$graph_time)
    }
  }

  # Remove all edges in selection
  graph$selection$edges <- NULL

  return(graph)
}
