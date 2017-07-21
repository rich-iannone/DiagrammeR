#' Get bridging scores
#' @description Get the bridging scores (based on
#' Valente's Bridging vertex measure) for all nodes
#' in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with bridging scores for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the bridging scores for nodes
#' # in the graph
#' get_bridging(graph)
#' #>    id    bridging
#' #> 1   1 0.006944444
#' #> 2   2 0.005555556
#' #> 3   3 0.006481481
#' #> 4   4 0.006790123
#' #> 5   5 0.005555556
#' #> 6   6 0.008201058
#' #> 7   7 0.006481481
#' #> 8   8 0.010185185
#' #> 9   9 0.005925926
#' #> 10 10 0.005925926
#'
#' # Add the bridging scores to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_bridging(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value    bridging
#' #> 1   1 <NA>     1   6.0 0.006944444
#' #> 2   2 <NA>     2   2.5 0.005555556
#' #> 3   3 <NA>     3   3.5 0.006481481
#' #> 4   4 <NA>     4   7.5 0.006790123
#' #> 5   5 <NA>     5   8.5 0.005555556
#' #> 6   6 <NA>     6   4.5 0.008201058
#' #> 7   7 <NA>     7  10.0 0.006481481
#' #> 8   8 <NA>     8  10.0 0.010185185
#' #> 9   9 <NA>     9   8.5 0.005925926
#' #> 10 10 <NA>    10  10.0 0.005925926
#' @importFrom influenceR bridging
#' @export get_bridging

get_bridging <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the betweeness scores for each of the
  # graph's nodes
  bridging_scores <- influenceR::bridging(ig_graph)

  # Create df with betweenness scores
  data.frame(
    id = names(bridging_scores),
    bridging = bridging_scores,
    stringsAsFactors = FALSE)
}
