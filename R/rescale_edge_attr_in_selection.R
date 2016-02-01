#' Rescale numeric edge attribute values for edges in a selection
#' @description From a graph object of class \code{dgr_graph}, take a set of
#' numeric values for edge attributes specified in a selection of edges,
#' rescale to a new numeric or color range, then write a new set of edge
#' attribute values.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr_from the edge attribute containing numeric data that
#' is to be rescaled to new numeric or color values.
#' @param edge_attr_to the name of the new or existing edge attribute that
#' will contain the scaled values. If the edge attribute exists for the
#' selected edges, rescaled values will replace any existing values.
#' @param to_lower_bound the lower bound value for the set of rescaled
#' values. This can be a numeric value or an X11 color name.
#' @param to_upper_bound the upper bound value for the set of rescaled
#' values. This can be a numeric value or an X11 color name.
#' @param from_lower_bound an optional, manually set lower bound value for
#' the set of values to be rescaled. If not set, the minimum value of the set of
#' values to be rescaled will be used.
#' @param from_upper_bound an optional, manually set upper bound value for
#' the set of values to be rescaled. If not set, the minimum value of the set of
#' values to be rescaled will be used.
#' @return a graph object of class \code{dgr_graph}.
#' @import scales
#' @importFrom grDevices colors
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
    get_edge_df(graph)[which(get_edge_df(graph)[,1]
                             %in% graph$selection$edges$from &
                               get_edge_df(graph)[,2]
                             %in% graph$selection$edges$to),]

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
                  values = edges_attr_vector_rescaled)

  return(graph)
}
