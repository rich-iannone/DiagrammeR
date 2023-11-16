#' Get Dice similarity coefficient scores
#'
#' @description
#'
#' Get the Dice similarity coefficient scores for one or more nodes in a graph.
#'
#' @inheritParams render_graph
#' @param nodes an optional vector of node IDs to consider for Dice similarity
#'   scores. If not supplied, then similarity scores will be provided for every
#'   pair of nodes in the graph.
#' @param direction using `all` (the default), the function will ignore
#'   edge direction when determining scores for neighboring nodes. With
#'   `out` and `in`, edge direction for neighboring nodes will be
#'   considered.
#' @param round_to the maximum number of decimal places to retain for the Dice
#'   similarity coefficient scores. The default value is `3`.
#'
#' @return A matrix with Dice similarity values for each pair of nodes
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
#' # Get the Dice similarity
#' # values for nodes `5`, `6`,
#' # and `7`
#' graph %>%
#'   get_dice_similarity(
#'     nodes = 5:7)
#'
#' @export
get_dice_similarity <- function(
    graph,
    nodes = NULL,
    direction = "all",
    round_to = 3
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  if (is.null(nodes)) {
    ig_nodes <- igraph::V(ig_graph)
  }

  if (!is.null(nodes)) {

    # Stop function if nodes provided not in
    # the graph
    if (!all(nodes %in% get_node_ids(graph))) {

      cli::cli_abort(
        "One or more nodes provided not in graph.")
    }

    # Get an igraph representation of node ID values
    ig_nodes <- igraph::V(ig_graph)[nodes]
  }

  # Get the Dice similarity scores
  if (direction == "all") {
    d_sim_values <-
      as.matrix(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "all",
          method = "dice"))
  }

  if (direction == "out") {
    d_sim_values <-
      as.matrix(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "out",
          method = "dice"))
  }

  if (direction == "in") {
    d_sim_values <-
      as.matrix(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "in",
          method = "dice"))
  }

  if (is.null(nodes)) {
    row.names(d_sim_values) <- graph$nodes_df$id
    colnames(d_sim_values) <- graph$nodes_df$id
  }

  if (!is.null(nodes)) {
    row.names(d_sim_values) <- graph$nodes_df$id[nodes]
    colnames(d_sim_values) <- graph$nodes_df$id[nodes]
  }

  # Round all values in matrix to set SD
  round(d_sim_values, round_to)
}
