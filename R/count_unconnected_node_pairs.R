#' Get the number of unconnected node pairs
#'
#' @description
#'
#' Get the number of unconnected node pairs. This works for directed graphs.
#'
#' @inheritParams render_graph
#'
#' @return A single numeric value representing the number of unconnected node
#'   pairs.
#'
#' @examples
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Get a count of unconnected node
#' # pairs in the graph
#' graph %>%
#'   count_unconnected_node_pairs()
#'
#' # Create a full graph and then
#' # count all unconnected node pairs
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   count_unconnected_node_pairs()
#'
#' @export
count_unconnected_node_pairs <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the number of unconnected node pairs
  # in the graph
  unname(unlist(igraph::dyad_census(ig_graph)["null"]))
}
