#' Delete a node from an existing graph object
#' @description From a graph object of class \code{dgr_graph}, delete an existing node by specifying its node ID.
#' @param graph a graph object of class \code{dgr_graph} that is created using \code{create_graph}.
#' @param node a node ID for the node to be deleted from the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Add two nodes
#' graph <- add_node(graph, node = "a")
#' graph <- add_node(graph, node = "b")
#'
#' # Delete a node
#' graph <- delete_node(graph, node = "a")
#' }
#' @export delete_node

delete_node <- function(graph,
                        node){

  # Verify that 'node' is given as a single value
  node_is_single_value <- ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE){
    stop("Only a single node can be deleted using 'delete_node'.")
  }

  # Stop function if node is not in the graph
  if (!(node %in% get_nodes(graph))){
    stop("The specified node is not available in the graph.")
  }

  # If single node in graph create an empty graph, retaining
  # all global attributes
  if (nrow(graph$nodes) == 1){

    # Create a revised graph and return that graph
    dgr_graph <-
      create_graph(nodes_df = NULL,
                   edges_df = NULL,
                   directed = graph$directed,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   graph_name = graph$graph_name,
                   graph_tz = graph$graph_tz,
                   graph_time = graph$graph_time)

    return(dgr_graph)
  }

  if (!is.null(graph$edges_df)){

    # If number of edges in graph is greater than one
    if (!is.null(graph$edges_df) & nrow(graph$edges_df > 1)){

      # Create a revised node data frame
      revised_nodes_df <- graph$nodes_df[-which(graph$nodes_df$nodes == node),]

      # Create a revised edge data frame
      if (nrow(graph$edges_df[-which((graph$edges_df$from == node) |
                                     (graph$edges_df$to == node)),]) == 0) {
        revised_edges_df <- graph$edges_df
      } else {
        revised_edges_df <- graph$edges_df[-which((graph$edges_df$from == node) |
                                                    (graph$edges_df$to == node)),]
      }

      # Create a revised graph and return that graph
      dgr_graph <-
        create_graph(nodes_df = revised_nodes_df,
                     edges_df = revised_edges_df,
                     directed = graph$directed,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_tz = graph$graph_tz,
                     graph_time = graph$graph_time)

      return(dgr_graph)
    }

    if (!is.null(graph$edges_df) & nrow(graph$edges_df <= 1)){

      # Create a revised node data frame
      revised_nodes_df <- graph$nodes_df[-which(graph$nodes_df$nodes == node),]

      # Create a revised graph and return that graph
      dgr_graph <-
        create_graph(nodes_df = revised_nodes_df,
                     edges_df = NULL,
                     directed = graph$directed,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_tz = graph$graph_tz,
                     graph_time = graph$graph_time)

      return(dgr_graph)
    }
  }

  if (is.null(graph$edges_df)){

    # Create a revised node data frame
    revised_nodes_df <- graph$nodes_df[-which(graph$nodes_df$nodes == node),]

    # Create a revised graph and return that graph
    dgr_graph <-
      create_graph(nodes_df = revised_nodes_df,
                   edges_df = NULL,
                   directed = graph$directed,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   graph_name = graph$graph_name,
                   graph_tz = graph$graph_tz,
                   graph_time = graph$graph_time)

    return(dgr_graph)
  }

  if (!is.null(graph$edges_df) & nrow(graph$edges_df <= 1)){

    # Create a revised node data frame
    revised_nodes_df <- graph$nodes_df[-which(graph$nodes_df$nodes == node),]

    # Create a revised graph and return that graph
    dgr_graph <-
      create_graph(nodes_df = revised_nodes_df,
                   edges_df = NULL,
                   directed = graph$directed,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   graph_name = graph$graph_name,
                   graph_tz = graph$graph_tz,
                   graph_time = graph$graph_time)

    return(dgr_graph)
  }
}
