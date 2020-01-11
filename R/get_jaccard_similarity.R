#' Get Jaccard similarity coefficient scores
#'
#' Get the Jaccard similarity coefficient scores for one or more nodes in a
#' graph.
#'
#' @inheritParams render_graph
#' @param nodes An optional vector of node IDs to consider for Jaccard
#'   similarity scores. If not supplied, then similarity scores will be provided
#'   for every pair of nodes in the graph.
#' @param direction Using `all` (the default), the function will ignore edge
#'   direction when determining scores for neighboring nodes. With `out` and
#'   `in`, edge direction for neighboring nodes will be considered.
#' @param round_to The maximum number of decimal places to retain for the
#'   Jaccard similarity coefficient scores. The default value is `3`.
#'
#' @return A matrix with Jaccard similarity values for each pair of nodes
#'   considered.
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
#' # Get the Jaccard similarity
#' # values for nodes `5`, `6`,
#' # and `7`
#' graph %>%
#'   get_jaccard_similarity(
#'     nodes = 5:7)
#'
#' @export
get_jaccard_similarity <- function(graph,
                                   nodes = NULL,
                                   direction = "all",
                                   round_to = 3) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  if (!(direction %in% c("all", "in", "out"))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "Valid options for `direction` are `all`, `in`, or `out`")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  if (is.null(nodes)) {
    ig_nodes <- igraph::V(ig_graph)
  }

  if (!is.null(nodes)) {

    # Stop function if nodes provided not in
    # the graph
    if (!all(nodes %in% get_node_ids(graph))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "One or more nodes provided not in graph")
    }

    # Get an igraph representation of node ID values
    ig_nodes <- igraph::V(ig_graph)[nodes]
  }

  # Get the Jaccard similarity scores
  if (direction == "all") {
    j_sim_values <-
      as.matrix(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "all",
          method = "jaccard"))
  }

  if (direction == "out") {
    j_sim_values <-
      as.matrix(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "out",
          method = "jaccard"))
  }

  if (direction == "in") {
    j_sim_values <-
      as.matrix(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "in",
          method = "jaccard"))
  }

  if (is.null(nodes)) {
    row.names(j_sim_values) <- graph$nodes_df$id
    colnames(j_sim_values) <- graph$nodes_df$id
  }

  if (!is.null(nodes)) {
    row.names(j_sim_values) <- graph$nodes_df$id[nodes]
    colnames(j_sim_values) <- graph$nodes_df$id[nodes]
  }

  # Round all values in matrix to set SD
  round(j_sim_values, round_to)
}
