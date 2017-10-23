#' Get the number of weakly-connected components
#' @description Get the number of weakly-connected
#' components in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single integer value representing the
#' number of weakly-connected graph components.
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5) %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of weakly-connected
#' # components in the graph
#' graph %>%
#'   count_w_connected_cmpts()
#' #> [1] 2
#' @importFrom igraph components
#' @export count_w_connected_cmpts

count_w_connected_cmpts <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the number of weakly-connected
  # components in the graph
  igraph::components(ig_graph, mode = "weak")[["no"]]
}
