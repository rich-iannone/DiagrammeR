#' Get betweenness centrality scores
#' @description Get the betweenness centrality scores
#' for all nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with betweenness scores for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the betweenness scores for nodes in the graph
#' get_betweenness(graph)
#' #>    id betweenness
#' #> 1   1         0.0
#' #> 2   2         0.0
#' #> 3   3         1.0
#' #> 4   4         0.0
#' #> 5   5         1.5
#' #> 6   6         7.5
#' #> 7   7         0.5
#' #> 8   8         0.0
#' #> 9   9         1.5
#' #> 10 10         0.0
#'
#' # Add the betweenness values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_betweenness(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value betweenness
#' #> 1   1 <NA>     1   6.0         0.0
#' #> 2   2 <NA>     2   2.5         0.0
#' #> 3   3 <NA>     3   3.5         1.0
#' #> 4   4 <NA>     4   7.5         0.0
#' #> 5   5 <NA>     5   8.5         1.5
#' #> 6   6 <NA>     6   4.5         7.5
#' #> 7   7 <NA>     7  10.0         0.5
#' #> 8   8 <NA>     8  10.0         0.0
#' #> 9   9 <NA>     9   8.5         1.5
#' #> 10 10 <NA>    10  10.0         0.0
#' @importFrom igraph betweenness
#' @export get_betweenness

get_betweenness <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the betweenness scores for each of the
  # graph's nodes
  betweenness_scores <-
    igraph::betweenness(
      graph = ig_graph,
      v = V(ig_graph),
      directed = graph$directed)

  # Create df with betweenness scores
  data.frame(
    id = betweenness_scores %>%
      names() %>%
      as.integer(),
    betweenness = betweenness_scores,
    stringsAsFactors = FALSE)
}
