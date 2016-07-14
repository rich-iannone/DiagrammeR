#' Get nodes within strongly connected components
#' @description Determine which nodes in a graph belong
#' to different strongly connected components.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a data frame with nodes and their membership
#' in different strongly connected components.
#' @importFrom igraph components
#' @export get_s_connected_components

get_s_connected_components <- function(graph) {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the component list
  components <-
    igraph::components(ig_graph, mode = 'strong')

  # Create the output data frame
  components_df <-
    data.frame(node = names(components$membership),
               sc_component = components$membership,
               stringsAsFactors = FALSE)

  return(components_df)
}
