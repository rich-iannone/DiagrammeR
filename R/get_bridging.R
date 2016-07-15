#' Get bridging scores
#' @description Get the bridging scores (based on
#' Valente's Bridging vertex measure) for all nodes in
#' a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with bridging scores for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the bridging scores for nodes in the graph
#' get_bridging(graph)
#' #>    node    bridging
#' #> 1     1 0.006944444
#' #> 2     2 0.006296296
#' #> 3     3 0.006018519
#' #> 4     4 0.006944444
#' #> 5     5 0.005555556
#' #> 6     6 0.006790123
#' #> 7     7 0.006790123
#' #> 8     8 0.006666667
#' #> 9     9 0.005555556
#' #> 10   10 0.006790123
#' @importFrom influenceR bridging
#' @export get_bridging

get_bridging <- function(graph) {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the betweeness scores for each of the
  # graph's nodes
  bridging_scores <- influenceR::bridging(ig_graph)

  # Create df with betweenness scores
  bridging_scores_df <-
    data.frame(node = names(bridging_scores),
               bridging = bridging_scores,
               stringsAsFactors = FALSE)

  return(bridging_scores_df)
}
