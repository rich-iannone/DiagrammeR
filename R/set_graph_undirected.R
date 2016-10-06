#' Convert graph to an undirected graph
#' @description Take a graph which is directed and
#' convert it to an undirected graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a directed tree
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2)
#'
#' # Convert this graph from directed to undirected
#' graph <- graph %>% set_graph_undirected
#'
#' # Perform a check on whether graph is directed
#' graph %>% is_graph_directed
#' #> [1] FALSE
#' @export set_graph_undirected

set_graph_undirected <- function(graph) {

  # If graph is already undirected, stop function
  if (graph$directed == FALSE) {
    stop("The graph is already undirected.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Convert graph to an undirected graph
  dgr_graph <-
    create_graph(
      nodes_df = graph$nodes_df,
      edges_df = graph$edges_df,
      graph_attrs = graph$graph_attrs,
      node_attrs = graph$node_attrs,
      edge_attrs = graph$edge_attrs,
      directed = FALSE,
      graph_name = graph$graph_name,
      graph_time = graph$graph_time,
      graph_tz = graph$graph_tz)

  # Update the `last_node` counter
  dgr_graph$last_node <- nodes_created

  return(dgr_graph)
}
