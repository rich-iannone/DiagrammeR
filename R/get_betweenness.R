#' Get betweenness centrality scores
#' @description Get the betweenness centrality scores
#' for all nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with betweenness scores for
#' each of the nodes.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 12,
#'     set_seed = 23)
#'
#' # Get the betweenness scores
#' # for nodes in the graph
#' graph %>%
#'   get_betweenness()
#'
#' # Add the betweenness
#' # values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_betweenness(.))
#'
#' # Display the graph's node
#' # data frame
#' graph %>%
#'   get_node_df()
#' @importFrom igraph betweenness V
#' @export get_betweenness

get_betweenness <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the betweenness scores for each of the
  # graph's nodes
  betweenness_scores <-
    igraph::betweenness(
      graph = ig_graph,
      v = igraph::V(ig_graph),
      directed = graph$directed)

  # Create df with betweenness scores
  data.frame(
    id = betweenness_scores %>%
      names() %>%
      as.integer(),
    betweenness = betweenness_scores,
    stringsAsFactors = FALSE)
}
