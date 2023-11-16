#' Get leverage centrality
#'
#' @description
#'
#' Get the leverage centrality values for all nodes in the graph. Leverage
#' centrality is a measure of the relationship between the degree of a given
#' node and the degree of each of its neighbors, averaged over all neighbors. A
#' node with negative leverage centrality is influenced by its neighbors, as the
#' neighbors connect and interact with far more nodes. A node with positive
#' leverage centrality influences its neighbors since the neighbors tend to have
#' far fewer connections.
#'
#' @inheritParams render_graph
#'
#' @return A data frame with leverage centrality values for each of the nodes.
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
#' # Get leverage centrality values
#' # for all nodes in the graph
#' graph %>%
#'   get_leverage_centrality()
#'
#' # Add the leverage centrality
#' # values to the graph as a
#' # node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_leverage_centrality(.))
#'
#' # Display the graph's node data frame
#' graph %>% get_node_df()
#'
#' @export
get_leverage_centrality <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get degree values for all graph nodes
  degree_vals <- igraph::degree(ig_graph)

  # Calculate leverage centrality values for
  # all nodes in the graph
  leverage_centrality_values <-
    purrr::map(
      seq_along(degree_vals),
      function(x) {
        mean(
          (degree_vals[x] - degree_vals[igraph::neighbors(ig_graph, degree_vals)]) /
            (degree_vals[x] + degree_vals[igraph::neighbors(ig_graph, degree_vals)]))
      }) %>%
    unlist()

  # Create df with leverage centrality values
  data.frame(
    id = degree_vals %>%
      names() %>%
      as.integer(),
    leverage_centrality = leverage_centrality_values %>% round(4),
    stringsAsFactors = FALSE)
}
