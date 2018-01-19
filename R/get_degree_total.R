#' Get total degree values for all nodes
#' @description Get the total degree values for all
#' nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param normalized set as \code{FALSE} (the default),
#' the total degree will be provided for each of
#' the nodes (as a count of edges to and from each
#' node). When set as \code{TRUE}, then the result for
#' each node will be divided by the total number of nodes
#' in the graph minus 1.
#' @return a data frame with total degree values for
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
#' # Get the total degree values
#' # for all nodes in the graph
#' graph %>%
#'   get_degree_total()
#'
#' # Add the total degree values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_degree_total(.))
#'
#' # Display the graph's
#' # node data frame
#' graph %>%
#'   get_node_df()
#' @importFrom igraph degree
#' @export get_degree_total

get_degree_total <- function(graph,
                             normalized = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the total degree values for each of the
  # graph's nodes
  if (normalized == TRUE) {
    total_degree_values <-
      igraph::degree(
        ig_graph,
        mode = "total",
        normalized = TRUE)
  }

  if (normalized == FALSE) {
    total_degree_values <-
      igraph::degree(
        ig_graph,
        mode = "total",
        normalized = FALSE)
  }

  # Create df with total degree scores
  data.frame(
    id = total_degree_values %>%
      names() %>%
      as.integer(),
    total_degree = total_degree_values,
    stringsAsFactors = FALSE)
}
