#' Get the authority scores for all nodes
#'
#' @description
#'
#' Get the Kleinberg authority centrality scores for all nodes in the graph.
#'
#' @inheritParams render_graph
#' @param weights_attr an optional name of the edge attribute to use in the
#' adjacency matrix. If `NULL` then, if it exists, the `weight` edge
#' attribute of the graph will be used.
#'
#' @return a data frame with authority scores for each of the nodes.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the authority centrality scores
#' # for all nodes in the graph
#' graph %>%
#'   get_authority_centrality()
#'
#' # Add the authority centrality
#' # scores to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_authority_centrality(.))
#'
#' # Display the graph's node data frame
#' graph %>% get_node_df()
#'
#' @export
get_authority_centrality <- function(
    graph,
    weights_attr = NULL
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # if weights_attr is not NULL and character
  if (inherits(weights_attr, "character")) {

    # Stop function if the edge attribute does not exist
    if (!(weights_attr %in% colnames(graph$edges_df))) {

      cli::cli_abort(
        "The edge attribute to be used as weights must exist in the graph.")
    }

    # Stop function if the edge attribute is not numeric
    if (!is.numeric(graph$edges_df[, which(colnames(graph$edges_df) == weights_attr)])) {

      cli::cli_abort(
        "The edge attribute to be used as weights must be numeric.")
    }

    weights_attr <- graph$edges_df[, which(colnames(graph$edges_df) == weights_attr)]
  }

  # Get the authority centrality values for
  # each of the graph's nodes
  authority_centrality_values <-
    igraph::authority_score(
      graph = ig_graph,
      weights = weights_attr)

  # Create df with authority centrality values
  data.frame(
    id = authority_centrality_values$vector %>%
      names() %>%
      as.integer(),
    authority_centrality = unname(authority_centrality_values$vector),
    stringsAsFactors = FALSE)
}
