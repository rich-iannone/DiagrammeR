#' Get indegree values for all nodes
#' @description Get the indegree values for all
#' nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param normalized set as \code{FALSE} (the default),
#' the indegree will be provided for each of
#' the nodes (as a count of edges to each node). When set
#' as \code{TRUE}, then the result for each node will be
#' divided by the total number of nodes in the graph minus 1.
#' @return a data frame with indegree values for
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
#' # Get the indegree values for
#' # all nodes in the graph
#' get_degree_in(graph)
#'
#' # Add the indegree values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_degree_in(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' @importFrom igraph degree
#' @export get_degree_in

get_degree_in <- function(graph,
                          normalized = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the indegree values for each of the
  # graph's nodes
  if (normalized == TRUE) {
    indegree_values <-
      igraph::degree(
        ig_graph,
        mode = "in",
        normalized = TRUE)
  }

  if (normalized == FALSE) {
    indegree_values <-
      igraph::degree(
        ig_graph,
        mode = "in",
        normalized = FALSE)
  }

  # Create df with indegree scores
  data.frame(
    id = indegree_values %>%
      names() %>%
      as.integer(),
    indegree = indegree_values,
    stringsAsFactors = FALSE)

}
