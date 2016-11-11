#' Get all nodes associated with connected components
#' @description Determine which nodes in a graph belong
#' to different connected components (i.e., distinct
#' sets of nodes with traversable paths to and from
#' each node in the set).
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a data frame with nodes and their membership
#' in different weakly connected components.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(36, 50, set_seed = 1)
#'
#' # Check if the graph is connected
#' is_graph_connected(graph)
#' #> [1] TRUE
#'
#' # Modify the graph so that it becomes disconnected
#' # (with two clusters of nodes)
#' graph <-
#'   graph %>%
#'   delete_edge(10, 36) %>%
#'   delete_edge(25, 27) %>%
#'   delete_edge(28, 29) %>%
#'   delete_edge(4, 29) %>%
#'   delete_edge(24, 32)
#'
#' # Verify that the graph is disconnected
#' is_graph_connected(graph)
#' #> [1] FALSE
#'
#' # Get the graph's connected components
#' get_w_connected_cmpts(graph)
#' #>    id wc_component
#' #> 1   1            1
#' #> 2   2            1
#' #> 3   3            1
#' #> 4   4            1
#' #> 5   5            1
#' #> 6   6            1
#' #> 7   7            1
#' #> 8   8            1
#' #> 9   9            1
#' #> 10 10            1
#' #> .. ..           ..
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
