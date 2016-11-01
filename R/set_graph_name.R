#' Set graph name
#' @description Set a name for a graph object of class
#' \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param name the name to set for the graph.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a name
#' graph <- set_graph_name(graph, "example_name")
#' @return a graph object of class \code{dgr_graph}.
#' @export set_graph_name

set_graph_name <- function(graph,
                           name) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  name <- as.character(name)

  dgr_graph <-
    create_graph(
      nodes_df = graph$nodes_df,
      edges_df = graph$edges_df,
      directed = ifelse(is_graph_directed(graph),
                        TRUE, FALSE),
      graph_name = name,
      graph_time = graph$graph_time,
      graph_tz = graph$graph_tz)

  if (!is.null(graph$selection)) {
    dgr_graph$selection <- graph$selection
  }

  # Update the `last_node` counter
  dgr_graph$last_node <- nodes_created

  # Update the `global_attrs` df
  dgr_graph$global_attrs <- graph$global_attrs

  return(dgr_graph)
}
