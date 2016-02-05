#' Deposit edge attributes (based on a selection of edges) in the graph
#' @description From a graph object of class \code{dgr_graph}, get edge
#' attribute properties for edges available in a selection and deposit
#' those values in the graph for later retrieval using \code{withdraw_values}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param edge_attr the edge attribute from which to obtain values.
#' @param mode a option to recast the returned vector of edge attribute
#' value as \code{numeric} or \code{character}.
#' @return a graph object of class \code{dgr_graph}.
#' @export deposit_edge_attr_from_selection

deposit_edge_attr_from_selection <- function(graph,
                                             edge_attr,
                                             mode = NULL){

  if (is.null(graph$selection$edges)){
    stop("There is no selection of edges available.")
  }

  edges_df <-
    get_edge_df(graph)[which(get_edge_df(graph)[,1]
                             %in% graph$selection$edges$from &
                               get_edge_df(graph)[,2]
                             %in% graph$selection$edges$to),]

  if (!is.null(edge_attr)){

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
}
