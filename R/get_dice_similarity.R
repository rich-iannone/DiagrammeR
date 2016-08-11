#' Get Dice similarity coefficient scores
#' @description Get the Dice similiarity coefficient
#' scores for one or more nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes an optional vector of node IDs to
#' consider for Dice similarity scores. If notsupplied,
#' then similarity scores will be provided for every
#' pair of nodes in the graph.
#' @param direction using \code{all} (the default), the
#' function will ignore edge direction when
#' determining scores for neighboring nodes. With
#' \code{out} and \code{in}, edge direction for
#' neighboring nodes will be considered.
#' @return a data frame with Dice similiarity values
#' for each pair of nodes considered.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the Dice similarity values for
#' # nodes `5`, `6`, and `7`
#' get_dice_similarity(graph, 5:7)
#' #>   node       n_5       n_6       n_7
#' #> 1    5 1.0000000 0.4444444 0.6666667
#' #> 2    6 0.4444444 1.0000000 0.4444444
#' #> 3    7 0.6666667 0.4444444 1.0000000
#' @importFrom igraph similarity V
#' @export get_dice_similarity

get_dice_similarity <- function(graph,
                                nodes = NULL,
                                direction = "all") {

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  if (is.null(nodes)) {
    ig_nodes <- V(ig_graph)
  }

  if (!is.null(nodes)) {

    # Stop function if nodes provided not in
    # the graph
    if (!all(as.character(nodes) %in%
             get_nodes(graph))) {
      stop("One or more nodes provided not in graph.")
    }

    # Get an igraph representation of node ID values
    ig_nodes <- V(ig_graph)[nodes]
  }

  # Get the Dice similarity scores
  if (direction == "all") {
    d_sim_values <-
      as.data.frame(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "all",
          method = "dice"))
  }

  if (direction == "out") {
    d_sim_values <-
      as.data.frame(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "out",
          method = "dice"))
  }

  if (direction == "in") {
    d_sim_values <-
      as.data.frame(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "in",
          method = "dice"))
  }

  # Create df with Dice similarity scores
  d_sim_values_df <-
    cbind(
      data.frame(node = get_node_df(graph)$nodes[nodes],
                 stringsAsFactors = FALSE),
      d_sim_values)

  # Modify the column names
  if (is.null(nodes)) {
    colnames(d_sim_values_df)[-1] <-
      paste0("n_", get_node_df(graph)$nodes)
  } else if (!is.null(nodes)) {
    colnames(d_sim_values_df)[-1] <-
      paste0("n_", get_node_df(graph)$nodes[nodes])
  }

  return(d_sim_values_df)
}
