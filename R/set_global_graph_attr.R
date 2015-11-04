#' Set global graph attributes
#' @description Set global attributes of a specific type (either
#' \code{graph_attrs}, \code{node_attrs}, or \code{edge_attrs} for a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param type the specific type of global graph attribute to set. The type is
#' specified with \code{graph}, \code{node}, or \code{edge}.
#' @param attr_for_type the name of the attribute to set for the \code{type}
#' of global attribute specified.
#' @param value the value to be set for the chosen attribute specified in the
#' \code{attr_for_type} argument.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a new graph and set some global attributes
#' graph <- create_graph() %>%
#'   set_global_graph_attr("graph", "overlap", "true") %>%
#'   set_global_graph_attr("node", "fontname", "Helvetica") %>%
#'   set_global_graph_attr("edge", "color", "gray")
#'
#' # Verify that the global attributes have been set
#' get_global_graph_attr(graph)
#' #> $graph_attrs
#' #> [1] "overlap = true"
#' #>
#' #> $node_attrs
#' #> [1] "fontname = Helvetica"
#' #>
#' #> $edge_attrs
#' #> [1] "color = gray"
#' }
#' @export set_global_graph_attr

set_global_graph_attr <- function(graph,
                                  type,
                                  attr_for_type,
                                  value){

  # Create a statement based on the attribute name and the value
  attr_stmt <- paste(attr_for_type, value, sep = " = ")

  if (type == "graph"){
    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = graph$edges_df,
                   graph_attrs = c(graph$graph_attrs, attr_stmt),
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)
  }

  if (type == "node"){
    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = graph$edges_df,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = c(graph$node_attrs, attr_stmt),
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)
  }

  if (type == "edge"){
    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = graph$edges_df,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs =c(graph$edge_attrs, attr_stmt),
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)
  }

  return(dgr_graph)
}
