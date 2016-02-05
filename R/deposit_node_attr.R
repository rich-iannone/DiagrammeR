#' Deposit node attributes in the graph
#' @description From a graph object of class \code{dgr_graph}, get node
#' attribute properties for one or more nodes and deposit those values
#' in the graph for later retrieval using \code{withdraw_values}.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param node_attr the node attribute from which to obtain values.
#' @param mode a option to recast the returned vector of node attribute
#' value as \code{numeric} or \code{character}.
#' @param nodes an optional vector of node IDs for filtering list of
#' nodes present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @export deposit_node_attr

deposit_node_attr <- function(graph,
                              node_attr,
                              mode = NULL,
                              nodes = NULL){


  nodes_df <- graph$nodes_df

  if (is.null(nodes) == TRUE){

    nodes_df <- nodes_df

  } else {

    nodes_df <-
      nodes_df[which(nodes_df$nodes %in% nodes),]
  }

  if (any(node_attr %in% colnames(nodes_df)[-1])){

    nodes_attr_vector <-
      nodes_df[,which(colnames(nodes_df) %in% node_attr)]

    if (!is.null(mode)){
      if (mode == "numeric"){
        nodes_attr_vector <- as.numeric(nodes_attr_vector)

        nodes_attr_vector <-
          nodes_attr_vector[which(!is.na(nodes_attr_vector))]
      }

      if (mode == "character"){
        nodes_attr_vector <- as.character(nodes_attr_vector)
      }
    }
  }

  # Place vector of node attributes as a deposit in the graph
  graph$deposit <- nodes_attr_vector

  return(graph)
}
