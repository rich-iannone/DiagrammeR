#' Add nodes from a node data frame to an existing graph object
#' @description With a graph object of class \code{dgr_graph} add nodes from a
#' node data frame to that graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_df a node data frame that is created using \code{create_nodes}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' }
#' @export add_node_df

add_node_df <- function(graph,
                        node_df){

  # Ensure that the nodes in the node data frame specified are in
  # the graph object
  all_nodes_in_graph <-
    all(node_df$nodes %in% get_nodes(graph))

  # If not all the nodes specified in the node data frame are in the
  # graph, stop the function
  if (all_nodes_in_graph == FALSE){
    stop("Not all nodes specified in the node data frame are in the graph.")
  }

  # If the 'nodes_df' component of the graph is not null, combine the
  # incoming node data frame with the existing node definitions in the
  # graph object
  if (!is.null(graph$nodes_df)){

    combined_nodes <- combine_nodes(graph$nodes_df,
                                    node_df)

    dgr_graph <-
      create_graph(nodes_df = combined_nodes,
                   edges_df = graph$edges_df,
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

  # If the 'nodes_df' component of the graph is null, insert the
  # node data frame into the graph object
  if (is.null(graph$nodes_df)){

    dgr_graph <-
      create_graph(nodes_df = node_df,
                   edges_df = graph$edges_df,
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
