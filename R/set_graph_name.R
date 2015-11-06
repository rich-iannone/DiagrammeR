#' Set graph name
#' @description Set a name for a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param name the name to set for the graph.
#' @examples
#' \dontrun{
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a name
#' graph <- set_graph_name(graph, "example_name")
#' }
#' @return a graph object of class \code{dgr_graph}.
#' @export set_graph_name

set_graph_name <- function(graph,
                           name){

  name <- as.character(name)

  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = graph$edges_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = ifelse(is_graph_directed(graph),
                                   TRUE, FALSE),
                 graph_name = name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  if (!is.null(graph$selection)){
    dgr_graph$selection <- graph$selection
  }

  return(dgr_graph)
}
