#' Delete an edge from an existing graph object
#' @description From a graph object of class \code{dgr_graph}, delete an
#' existing edge by specifying a pair of node IDs corresponding to the edge
#' direction.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param from a node ID from which the edge to be removed is outgoing.
#' @param to a node ID to which the edge to be removed is incoming.
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
#' # Add an edge
#' graph <- add_edge(graph, from = "a", to = "b")
#'
#' # Delete the edge
#' graph <- delete_edge(graph, from = "a", to = "b")
#' }
#' @export delete_edge

delete_edge <- function(graph,
                        from,
                        to){

  # Verify that each of the values for 'from' and 'to' are given as
  # single values
  from_is_single_value <- ifelse(length(from) == 1, TRUE, FALSE)
  to_is_single_value <- ifelse(length(to) == 1, TRUE, FALSE)

  # Stop function if either node is not a single value
  if (from_is_single_value == FALSE | to_is_single_value == FALSE){

    stop("Only single nodes for 'from' and 'to' should be specified.")
  }

  # Determine whether pair of node provided are in the graph
  if (from_is_single_value == TRUE & to_is_single_value == TRUE){

    nodes_available_in_graph <-
      ifelse(all(c(from, to) %in% get_nodes(graph)), TRUE, FALSE)
  }

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE){

    stop("The nodes specified are not both present in the graph.")
  }

  # Determine whether a matching edge is available in the graph
  if (nodes_available_in_graph){

    if (any(graph$edges_df$from == from & graph$edges_df$to == to)){

      row_id_edge_removal <-
        which(graph$edges_df$from == from & graph$edges_df$to == to)

      revised_edges_df <- graph$edges_df[-row_id_edge_removal,]

      row.names(revised_edges_df) <- NULL

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = revised_edges_df,
                     directed = graph$directed,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     graph_name = graph$graph_name,
                     graph_tz = graph$graph_tz,
                     graph_time = graph$graph_time
        )
    }
  }

  return(dgr_graph)
}
