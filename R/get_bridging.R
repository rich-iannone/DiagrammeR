#' Get bridging scores
#' @description Get the bridging scores (based on
#' Valente's Bridging vertex measure) for all nodes
#' in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with bridging scores for
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
#' # Get the bridging scores for nodes
#' # in the graph
#' get_bridging(graph)
#' #>    id    bridging
#' #> 1   1 0.018148148
#' #> 2   2 0.034953704
#' #> 3   3 0.008024691
#' #> 4   4 0.020814815
#' #> 5   5 0.018209877
#' #> 6   6 0.044444444
#' #> 7   7 0.046296296
#' #> 8   8 0.019537037
#' #> 9   9 0.044444444
#' #> 10 10 0.007870370
#'
#' # Add the bridging scores to
#' # the graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_bridging(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label    bridging
#' #> 1   1 <NA>  <NA> 0.018148148
#' #> 2   2 <NA>  <NA> 0.034953704
#' #> 3   3 <NA>  <NA> 0.008024691
#' #> 4   4 <NA>  <NA> 0.020814815
#' #> 5   5 <NA>  <NA> 0.018209877
#' #> 6   6 <NA>  <NA> 0.044444444
#' #> 7   7 <NA>  <NA> 0.046296296
#' #> 8   8 <NA>  <NA> 0.019537037
#' #> 9   9 <NA>  <NA> 0.044444444
#' #> 10 10 <NA>  <NA> 0.007870370
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
    id = bridging_scores %>%
      names() %>%
      as.integer(),
    bridging = bridging_scores,
    stringsAsFactors = FALSE)
}
