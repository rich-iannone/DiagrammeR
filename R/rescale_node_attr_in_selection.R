#' Rescale numeric node attribute values for nodes in a selection
#' @description From a graph object of class \code{dgr_graph}, take a set of
#' numeric values for node attributes specified in a selection of nodes,
#' rescale to a new numeric or color range, then write a new set of node
#' attribute values.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr_from the node attribute containing numeric data that
#' is to be rescaled to new numeric or color values.
#' @param node_attr_to the name of the new or existing node attribute that
#' will contain the scaled values. If the node attribute exists for the
#' selected nodes, rescaled values will replace any existing values.
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
#' @export rescale_node_attr_in_selection

rescale_node_attr_in_selection <- function(graph,
                                           node_attr_from,
                                           node_attr_to,
                                           to_lower_bound = 0,
                                           to_upper_bound = 1,
                                           from_lower_bound = NULL,
                                           from_upper_bound = NULL){

  # Get node attributes for the selected nodes
  nodes_df <-
    get_node_df(graph)[which(get_node_df(graph)[,1]
                             %in% graph$selection$nodes),]

  if (!any(node_attr_from %in% colnames(nodes_df)[-1])){
    stop("The node attribute to scale isn't present in the ndf.")
  }

  nodes_attr_vector <-
    nodes_df[,which(colnames(nodes_df) %in% node_attr_from)]

  nodes_attr_vector <- as.numeric(nodes_attr_vector)

  if ((!is.null(from_lower_bound) & is.null(from_upper_bound)) |
      (is.null(from_lower_bound) & !is.null(from_upper_bound)) |
      (is.null(from_lower_bound) & is.null(from_upper_bound))){

    from <- range(nodes_attr_vector, na.rm = TRUE, finite = TRUE)
  } else {
    from <- c(from_lower_bound, from_upper_bound)
  }

  # Get vector of rescaled, numeric node attribute values
  if (is.numeric(to_lower_bound) & is.numeric(to_upper_bound)){

    nodes_attr_vector_rescaled <-
      round(rescale(x = nodes_attr_vector,
                    to = c(to_lower_bound, to_upper_bound),
                    from = from), 3)
  }

  # Get vector of rescaled, node attribute color values
  if ((to_lower_bound %in% colors()) &
      (to_upper_bound %in% colors())){

    nodes_attr_vector_rescaled <-
      cscale(x = nodes_attr_vector,
             palette = seq_gradient_pal(to_lower_bound,
                                        to_upper_bound))
  }

  # Set the node attribute values for nodes specified in selection
  graph <-
    set_node_attr(x = graph,
                  nodes = graph$selection$nodes,
                  node_attr = node_attr_to,
                  values = nodes_attr_vector_rescaled)

  return(graph)
}
