#' Get betweenness centrality scores
#' @description Get the betweenness centrality scores
#' for all nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with betweenness scores for
#' each of the nodes.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the betweenness scores for nodes in the graph
#' get_betweenness(graph)
#' #>    node betweenness
#' #> 1     1    6.633333
#' #> 2     2    5.638095
#' #> 3     3    1.904762
#' #> 4     4    4.019048
#' #> 5     5    8.157143
#' #> 6     6    2.000000
#' #> 7     7   10.157143
#' #> 8     8    8.857143
#' #> 9     9    3.466667
#' #> 10   10    1.166667
#'
#' # Add the betweenness values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     get_betweenness(.),
#'     by_graph = "nodes",
#'     by_df = "node")
#' @importFrom influenceR betweenness
#' @export get_betweenness

get_betweenness <- function(graph) {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the betweenness scores for each of the
  # graph's nodes
  betweenness_scores <-
    influenceR::betweenness(ig_graph)

  # Create df with betweenness scores
  betweenness_scores_df <-
    data.frame(node = names(betweenness_scores),
               betweenness = betweenness_scores,
               stringsAsFactors = FALSE)

  return(betweenness_scores_df)
}
