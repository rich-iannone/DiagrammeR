#' Get the alpha centrality for all nodes
#'
#' Get the alpha centrality values for all nodes in the graph.
#' @inheritParams render_graph
#' @param alpha the parameter that specifies the relative importance of
#'   endogenous versus exogenous factors in the determination of centrality.
#' @param exo the exogenous factors, in most cases this is either a constant
#'   (which applies the same factor to every node), or a vector giving the
#'   factor for every node.
#' @param weights_attr an optional name of the edge attribute to use in the
#'   adjacency matrix. If `NULL` then, if it exists, the `weight` edge
#'   attribute of the graph will be used. Failing that, the standard adjacency
#'   matrix will be used in calculations.
#' @param tol the tolerance for near-singularities during matrix inversion.
#' The default value is set to `1e-7`.
#'
#' @return A data frame with alpha centrality scores for each of the nodes.
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
#' # Get the alpha centrality scores
#' # for all nodes
#' graph %>%
#'   get_alpha_centrality()
#'
#' # Add the alpha centrality
#' # scores to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_alpha_centrality(.))
#'
#' # Display the graph's node
#' # data frame
#' graph %>% get_node_df()
#'
#' @export
get_alpha_centrality <- function(graph,
                                 alpha = 1,
                                 exo = 1,
                                 weights_attr = NULL,
                                 tol = 1e-7) {

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

  # Get the alpha centrality values for each of the
  # graph's nodes
  alpha_centrality_values <-
    igraph::alpha_centrality(
      graph = ig_graph,
      alpha = alpha,
      exo = exo,
      weights = weights_attr,
      tol = tol)

  # Create df with alpha centrality values
  data.frame(
    id = alpha_centrality_values %>%
      names() %>%
      as.integer(),
    alpha_centrality = alpha_centrality_values,
    stringsAsFactors = FALSE)
}
