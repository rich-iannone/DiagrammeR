#' Get the number of weakly-connected components
#'
#' @description
#'
#' Get the number of weakly-connected components in the graph.
#'
#' @inheritParams render_graph
#'
#' @return A single integer value representing the number of weakly-connected
#'   graph components.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5) %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of weakly-connected
#' # components in the graph
#' graph %>% count_w_connected_cmpts()
#'
#' @export
count_w_connected_cmpts <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(NA_real_)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the number of weakly-connected
  # components in the graph
  igraph::components(ig_graph, mode = "weak")[["no"]]
}
