#' Get coreness values for graph nodes
#'
#' @description
#'
#' Get the coreness values for all nodes in a graph.
#'
#' @inheritParams render_graph
#' @param direction using `all` (the default), the search will ignore edge
#'   direction while traversing through the graph. With `out`, measurements
#'   of paths will be from a node whereas with `in`, measurements of paths
#'   will be to a node.
#'
#' @return A data frame with coreness values for each of the nodes.
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
#' # Get coreness values for
#' # all nodes in the graph
#' graph %>% get_coreness()
#'
#' # Add the coreness values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_coreness(.))
#'
#' # Display the graph's node data frame
#' graph %>% get_node_df()
#'
#' @export
get_coreness <- function(
    graph,
    direction = "all"
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  arg_match0(direction, c("all", "in", "out"))

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the coreness values for each of the
  # graph's nodes
  if (direction == "all") {
    coreness_values <-
      igraph::coreness(ig_graph, mode = "all")
  }

  if (direction == "out") {
    coreness_values <-
      igraph::coreness(ig_graph, mode = "out")
  }

  if (direction == "in") {
    coreness_values <-
      igraph::coreness(ig_graph, mode = "in")
  }

  # Create df with coreness values
  data.frame(
    id = coreness_values %>%
      names() %>%
      as.integer(),
    coreness = coreness_values,
    stringsAsFactors = FALSE)
}
