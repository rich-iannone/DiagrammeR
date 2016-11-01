#' Delete an edge from an existing graph object
#' @description From a graph object of class
#' \code{dgr_graph}, delete an existing edge by
#' specifying a pair of node IDs corresponding to the
#' edge direction.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param from a node ID from which the edge to be
#' removed is outgoing.
#' @param to a node ID to which the edge to be removed
#' is incoming.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Add two nodes
#' graph <- add_node(graph)
#' graph <- add_node(graph)
#'
#' # Add an edge
#' graph <-
#'   add_edge(
#'     graph = graph,
#'     from = 1,
#'     to = 2)
#'
#' # Delete the edge
#' graph <-
#'   delete_edge(
#'     graph = graph,
#'     from = 1,
#'     to = 2)
#' @export delete_edge

delete_edge <- function(graph,
                        from,
                        to) {

  # Verify that each of the values for `from` and
  # `to` are given as single values
  from_is_single_value <-
    ifelse(length(from) == 1, TRUE, FALSE)

  to_is_single_value <-
    ifelse(length(to) == 1, TRUE, FALSE)

  # Stop function if either node is not a single value
  if (from_is_single_value == FALSE |
      to_is_single_value == FALSE) {
    stop("Only single nodes for 'from' and 'to' should be specified.")
  }

  # Determine whether the pair of nodes provided
  # are in the graph
  if (from_is_single_value &
      to_is_single_value) {
    nodes_available_in_graph <-
      ifelse(all(c(from, to) %in%
                   get_node_ids(graph)),
             TRUE, FALSE)
  }

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE) {
    stop("The nodes specified are not both present in the graph.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Edge removal case for directed graphs
  if (nodes_available_in_graph &
      is_graph_directed(graph)) {

    if (any(graph$edges_df$from == from &
            graph$edges_df$to == to)) {

      row_id_edge_removal <-
        which(graph$edges_df$from == from &
                graph$edges_df$to == to)

      revised_edges_df <-
        graph$edges_df[-row_id_edge_removal,]

      row.names(revised_edges_df) <- NULL

      dgr_graph <-
        create_graph(
          nodes_df = graph$nodes_df,
          edges_df = revised_edges_df,
          directed = graph$directed,
          graph_name = graph$graph_name,
          graph_tz = graph$graph_tz,
          graph_time = graph$graph_time)

      # Update the `last_node` counter
      dgr_graph$last_node <- nodes_created

      # Update the `global_attrs` df
      dgr_graph$global_attrs <- graph$global_attrs
    }
  }

  # Edge removal case for undirected graphs
  if (nodes_available_in_graph &
      is_graph_directed(graph) == FALSE) {

    if (any(graph$edges_df$from == from &
            graph$edges_df$to == to)) {

      row_id_edge_removal <-
        which(graph$edges_df$from == from &
                graph$edges_df$to == to)

      revised_edges_df <-
        graph$edges_df[-row_id_edge_removal,]

      row.names(revised_edges_df) <- NULL

      dgr_graph <-
        create_graph(
          nodes_df = graph$nodes_df,
          edges_df = revised_edges_df,
          directed = graph$directed,
          graph_name = graph$graph_name,
          graph_tz = graph$graph_tz,
          graph_time = graph$graph_time)

      # Update the `last_node` counter
      dgr_graph$last_node <- nodes_created

      # Update the `global_attrs` df
      dgr_graph$global_attrs <- graph$global_attrs
    }

    if (any(graph$edges_df$from == to &
            graph$edges_df$to == from)) {

      row_id_edge_removal <-
        which(graph$edges_df$from == to &
                graph$edges_df$to == from)

      revised_edges_df <-
        graph$edges_df[-row_id_edge_removal,]

      row.names(revised_edges_df) <- NULL

      dgr_graph <-
        create_graph(
          nodes_df = graph$nodes_df,
          edges_df = revised_edges_df,
          directed = graph$directed,
          graph_name = graph$graph_name,
          graph_tz = graph$graph_tz,
          graph_time = graph$graph_time)

      # Update the `last_node` counter
      dgr_graph$last_node <- nodes_created

      # Update the `global_attrs` df
      dgr_graph$global_attrs <- graph$global_attrs
    }
  }

  return(dgr_graph)
}
