#' Set edge attributes based on a selection of edges
#' @description From a graph object of class \code{dgr_graph}, set edge
#' attribute properties for edges available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param edge_attr the name of the attribute to set.
#' @param value the value to be set for the chosen attribute for the
#' chosen edges.
#' @return a graph object of class \code{dgr_graph}.
#' @export set_edge_attr_with_selection

set_edge_attr_with_selection <- function(graph,
                                         edge_attr,
                                         value){

  if (is.null(graph$selection$edges)){
    stop("There is no selection of edges available.")
  }

  if (edge_attr == "from" | edge_attr == "to"){
    stop("You cannot alter values associated with node IDs.")
  }

  if (length(value) > 1){
    stop("Only one value should be provided.")
  }

  edges_df <- graph$edges_df

  if (edge_attr %in% colnames(edges_df)){

    graph_edges <-
      paste(graph$edges_df$from,
            "->",
            graph$edges_df$to)

    selection_edges <-
      paste(graph$selection$edges$from,
            "->",
            graph$selection$edges$to)

    edges_df[which(graph_edges %in% selection_edges),
             which(colnames(edges_df) %in% edge_attr)] <- value
  }

  if (!(edge_attr %in% colnames(edges_df))){

    edges_df <- cbind(edges_df, rep("", nrow(edges_df)))

    edges_df[,ncol(edges_df)] <- as.character(edges_df[,ncol(edges_df)])

    colnames(edges_df)[ncol(edges_df)] <- edge_attr

    graph_edges <-
      paste(graph$edges_df$from,
            "->",
            graph$edges_df$to)

    selection_edges <-
      paste(graph$selection$edges$from,
            "->",
            graph$selection$edges$to)

    edges_df[which(graph_edges %in% selection_edges),
             ncol(edges_df)] <- value
  }

  # Create new graph object while retaining the selection
  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = edges_df,
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
