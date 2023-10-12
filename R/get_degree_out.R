#' Get outdegree values for all nodes
#'
#' @description
#'
#' Get the outdegree values for all nodes in a graph.
#'
#' @inheritParams render_graph
#' @param normalized set as `FALSE` (the default), the outdegree will be
#'   provided for each of the nodes (as a count of edges outgoing from each
#'   node). When set as `TRUE`, then the result for each node will be
#'   divided by the total number of nodes in the graph minus 1.
#'
#' @return A data frame with outdegree values for each of the nodes.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the outdegree values
#' # for all nodes in the graph
#' graph %>%
#'   get_degree_out()
#'
#' # Add the outdegree values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_degree_out(.))
#'
#' # Display the graph's
#' # node data frame
#' graph %>% get_node_df()
#'
#' @export
get_degree_out <- function(
    graph,
    normalized = FALSE
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (!graph_object_valid(graph)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the outdegree values for each of the
  # graph's nodes
  if (normalized) {
    outdegree_values <-
      igraph::degree(
        ig_graph,
        mode = "out",
        normalized = TRUE)
  }

  if (!normalized) {
    outdegree_values <-
      igraph::degree(
        ig_graph,
        mode = "out",
        normalized = FALSE)
  }

  # Create df with outdegree scores
  data.frame(
    id = outdegree_values %>%
      names() %>%
      as.integer(),
    outdegree = outdegree_values,
    stringsAsFactors = FALSE)
}
