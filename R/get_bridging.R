#' Get bridging scores
#'
#' Get the bridging scores (based on Valente's Bridging vertex measure) for all
#' nodes in a graph.
#' @inheritParams render_graph
#'
#' @return A data frame with bridging scores for each of the nodes.
#'
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
#' graph %>% get_bridging()
#'
#' # Add the bridging scores to
#' # the graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_bridging(.))
#'
#' # Display the graph's node data frame
#' graph %>% get_node_df()
#'
#' @export
get_bridging <- function(graph) {

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
