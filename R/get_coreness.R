#' Get coreness values for graph nodes
#' @description Get the coreness values for all nodes
#' in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param direction using \code{all} (the default), the
#' search will ignore edge direction while traversing
#' through the graph. With \code{out}, measurements of
#' paths will be from a node whereas with \code{in},
#' measurements of paths will be to a node.
#' @return a data frame with coreness values for
#' each of the nodes.
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
#' get_coreness(graph)
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
#' get_node_df(graph)
#' @importFrom igraph coreness
#' @export get_coreness

get_coreness <- function(graph,
                         direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  if (!(direction %in% c("all", "in", "out"))) {

    stop(
      "Valid options for `direction` are `all`, `in`, or `out`.",
      call. = FALSE)
  }

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
