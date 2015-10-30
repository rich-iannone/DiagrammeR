#' Set node attributes based on a selection of nodes
#' @description From a graph object of class \code{dgr_graph}, set node
#' attribute properties for nodes available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param node_attr the name of the attribute to set.
#' @param value the value to be set for the chosen attribute for the
#' chosen nodes.
#' @return either a graph object of class \code{dgr_graph}.
#' @export set_node_attr_with_selection

set_node_attr_with_selection <- function(graph,
                                         node_attr,
                                         value){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  if (node_attr == "nodes"){
    stop("You cannot change the node ID.")
  }

  if (length(value) > 1){
    stop("Only one value should be provided.")
  }

  nodes_df <- graph$nodes_df

  if (node_attr %in% colnames(nodes_df)){

    nodes_df[which(nodes_df$nodes %in% graph$selection$nodes),
             which(colnames(nodes_df) %in% node_attr)] <- value
  }

  if (!(node_attr %in% colnames(nodes_df))){

    nodes_df <- cbind(nodes_df, rep("", nrow(nodes_df)))

    nodes_df[,ncol(nodes_df)] <- as.character(nodes_df[,ncol(nodes_df)])

    colnames(nodes_df)[ncol(nodes_df)] <- node_attr

    nodes_df[which(nodes_df$nodes %in% graph$selection$nodes),
             ncol(nodes_df)] <- value
  }

  # Create new graph object while retaining the selection
  dgr_graph <-
    create_graph(nodes_df = nodes_df,
                 edges_df = graph$edges_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = ifelse(is_graph_directed(graph),
                                   TRUE, FALSE),
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  dgr_graph$selection <- graph$selection

  return(dgr_graph)
}
