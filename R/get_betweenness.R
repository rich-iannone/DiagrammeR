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
#'     10, 22, set_seed = 23)
#'
#' # Get the betweenness scores for nodes in the graph
#' get_betweenness(graph)
#' #>    id betweenness
#' #> 1   1    5.904762
#' #> 2   2    4.904762
#' #> 3   3    1.785714
#' #> 4   4    0.000000
#' #> 5   5    5.738095
#' #> 6   6   20.523810
#' #> 7   7    3.333333
#' #> 8   8    0.000000
#' #> 9   9    3.738095
#' #> 10 10    4.071429
#'
#' # Add the betweenness values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     get_betweenness(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#'    id type label value betweenness
#' 1   1 <NA>     1   6.0    5.904762
#' 2   2 <NA>     2   2.5    4.904762
#' 3   3 <NA>     3   3.5    1.785714
#' 4   4 <NA>     4   7.5    0.000000
#' 5   5 <NA>     5   8.5    5.738095
#' 6   6 <NA>     6   4.5   20.523810
#' 7   7 <NA>     7  10.0    3.333333
#' 8   8 <NA>     8  10.0    0.000000
#' 9   9 <NA>     9   8.5    3.738095
#' 10 10 <NA>    10  10.0    4.071429
#' @importFrom influenceR betweenness
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
    influenceR::betweenness(ig_graph)

  # Create df with betweenness scores
  betweenness_scores_df <-
    data.frame(id = names(betweenness_scores),
               betweenness = betweenness_scores,
               stringsAsFactors = FALSE)

  return(betweenness_scores_df)
}
