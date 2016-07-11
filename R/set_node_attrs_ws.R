#' Set node attributes with a node selection
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, set node
#' attribute properties for nodes present in a node
#' selection.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr the name of the attribute to set.
#' @param value the value to be set for the chosen
#' attribute for the nodes in the current selection.
#' @return a graph object of class \code{dgr_graph}.
#' @export set_node_attrs_ws

set_node_attrs_ws <- function(graph,
                              node_attr,
                              value) {

  nodes_df <- graph$nodes_df
  nodes <- graph$selection$nodes

  if (node_attr %in% colnames(nodes_df)) {

    nodes_df[which(nodes_df$nodes %in% nodes),
             which(colnames(nodes_df) %in%
                     node_attr)] <- value
  }

  if (!(node_attr %in% colnames(nodes_df))) {

    nodes_df <-
      cbind(nodes_df, rep("", nrow(nodes_df)))

    nodes_df[, ncol(nodes_df)] <-
      as.character(nodes_df[,ncol(nodes_df)])

    colnames(nodes_df)[ncol(nodes_df)] <- node_attr

    nodes_df[
      which(nodes_df$nodes %in%
              nodes), ncol(nodes_df)] <- value
  }

  # Create new graph object
  dgr_graph <-
    create_graph(
      nodes_df = nodes_df,
      edges_df = graph$edges_df,
      graph_attrs = graph$graph_attrs,
      node_attrs = graph$node_attrs,
      edge_attrs = graph$edge_attrs,
      directed = ifelse(is_graph_directed(graph),
                        TRUE, FALSE),
      graph_name = graph$graph_name,
      graph_time = graph$graph_time,
      graph_tz = graph$graph_tz)

  # Retain the node selection in the graph
  dgr_graph$selection <- graph$selection

  return(dgr_graph)
}
