#' Get all nodes associated with connected components
#' @description Determine which nodes in a graph belong
#' to different weakly connected components (i.e.,
#' distinct sets of nodes with traversable paths to and
#' from each node in the set).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with nodes and their membership
#' in different weakly connected components.
#' @examples
#' # Create a graph with 2 cycles
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 4) %>%
#'   add_cycle(n = 3)
#'
#' # Check if the graph is connected
#' is_graph_connected(graph)
#' #> [1] FALSE
#'
#' # Get the graph's weakly-connected
#' # components
#' get_w_connected_cmpts(graph)
#' #>   id wc_component
#' #> 1  1            1
#' #> 2  2            1
#' #> 3  3            1
#' #> 4  4            1
#' #> 5  5            2
#' #> 6  6            2
#' #> 7  7            2
#' @importFrom igraph components
#' @export get_w_connected_cmpts

get_w_connected_cmpts <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the component list
  components <-
    igraph::components(ig_graph, "weak")

  # Create the output data frame
  components_df <-
    data.frame(
      id = as.integer(names(components$membership)),
      wc_component = components$membership,
      stringsAsFactors = FALSE)

  return(components_df)
}
