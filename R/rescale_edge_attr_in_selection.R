#' Rescale numeric edge attribute values for edges in a selection
#' @description From a graph object of class \code{dgr_graph}, take a set of
#' numeric values for edge attributes specified in a selection of edges,
#' rescale, then write a new set of edge attribute values.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr_from edge_attr_from
#' @param edge_attr_to edge_attr_to
#' @param to_lower_bound to_lower_bound
#' @param to_upper_bound to_upper_bound
#' @param from_lower_bound from_lower_bound
#' @param from_upper_bound from_upper_bound
#' @return a graph object of class \code{dgr_graph}.
#' @import scales
#' @export rescale_edge_attr_in_selection

rescale_edge_attr_in_selection <- function(graph,
                                           edge_attr_from,
                                           edge_attr_to,
                                           to_lower_bound = 0,
                                           to_upper_bound = 1,
                                           from_lower_bound = NULL,
                                           from_upper_bound = NULL){

  # Get edge attributes for the selected edges
  edges_df <-
    get_edge_attr(graph,
                  from = graph$selection$edges$from,
                  to = graph$selection$edges$to)

  if (!any(edge_attr_from %in% colnames(edges_df)[-c(1:2)])){
    stop("The edge attribute to scale isn't present in the edf.")
  }

  edges_attr_vector <-
    edges_df[,which(colnames(edges_df) %in% edge_attr_from)]

  edges_attr_vector <- as.numeric(edges_attr_vector)

  if ((!is.null(from_lower_bound) & is.null(from_upper_bound)) |
      (is.null(from_lower_bound) & !is.null(from_upper_bound)) |
      (is.null(from_lower_bound) & is.null(from_upper_bound))){

    from <- range(edges_attr_vector, na.rm = TRUE, finite = TRUE)
  } else {
    from <- c(from_lower_bound, from_upper_bound)
  }

  # Get vector of rescaled, numeric edge attribute values
  if (is.numeric(to_lower_bound) & is.numeric(to_upper_bound)){

    edges_attr_vector_rescaled <-
      round(rescale(x = edges_attr_vector,
                    to = c(to_lower_bound, to_upper_bound),
                    from = from), 3)
  }

  # Get vector of rescaled, edge attribute color values
  if ((to_lower_bound %in% colors()) &
      (to_upper_bound %in% colors())){

    edges_attr_vector_rescaled <-
      cscale(x = edges_attr_vector,
             palette = seq_gradient_pal(to_lower_bound,
                                        to_upper_bound))
  }

  # Set the edge attribute values for edges specified in selection
  graph <-
    set_edge_attr(x = graph,
                  from = graph$selection$edges$from,
                  to = graph$selection$edges$to,
                  edge_attr = edge_attr_to,
                  value = edges_attr_vector_rescaled)

  return(graph)
}
