#' Deposit edge attributes in the graph
#' @description From a graph object of class \code{dgr_graph}, get edge
#' attribute properties for one or more edges and deposit those values
#' in the graph for later retrieval using \code{withdraw_values}.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param edge_attr the edge attribute from which to obtain values.
#' @param mode a option to recast the returned vector of edge attribute
#' value as \code{numeric} or \code{character}.
#' @param from an optional vector of node IDs from which the edge is
#' outgoing for filtering the list of edges present in the graph.
#' @param to an optional vector of node IDs to which the edge is
#' incoming for filtering the list of edges present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @export deposit_edge_attr

deposit_edge_attr <- function(graph,
                              edge_attr,
                              mode = NULL,
                              from = NULL,
                              to = NULL){

  edges_df <- graph$edges_df

  if (is.null(from) & !is.null(to)){

    edges_df <-
      edges_df[which(edges_df$to %in% to),]

  } else if (!is.null(from) & is.null(to)){

    edges_df <-
      edges_df[which(edges_df$from %in% from),]

  } else if (is.null(from) & is.null(to)){

    edges_df <- edges_df

  } else {

    edges_df <-
      edges_df[which((edges_df$from %in% from) &
                       (edges_df$to %in% to)),]
  }

  if (any(edge_attr %in% colnames(edges_df)[-c(1:2)])){

    edges_attr_vector <-
      edges_df[,which(colnames(edges_df) %in% edge_attr)]

    if (!is.null(mode)){
      if (mode == "numeric"){
        edges_attr_vector <- as.numeric(edges_attr_vector)

        edges_attr_vector <-
          edges_attr_vector[which(!is.na(edges_attr_vector))]
      }

      if (mode == "character"){
        edges_attr_vector <- as.character(edges_attr_vector)
      }
    }
  }

  # Place vector of edge attributes as a deposit in the graph
  graph$deposit <- edges_attr_vector

  return(graph)

}
