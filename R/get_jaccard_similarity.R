#' Get Jaccard similarity coefficient scores
#' @description Get the Jaccard similiarity coefficient
#' scores for one or more nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes an optional vector of node IDs to
#' consider for Jaccard similarity scores. If not
#' supplied, then similarity scores will be provided
#' for every pair of nodes in the graph.
#' @param direction using \code{all} (the default), the
#' function will ignore edge direction when
#' determining scores for neighboring nodes. With
#' \code{out} and \code{in}, edge direction for
#' neighboring nodes will be considered.
#' @return a data frame with Jaccard similiarity values
#' for each pair of nodes considered.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the Jaccard similarity values for
#' # nodes `5`, `6`, and `7`
#' get_jaccard_similarity(graph, 5:7)
#' #>   node       n_5       n_6       n_7
#' #> 1    5 1.0000000 0.2857143 0.5000000
#' #> 2    6 0.2857143 1.0000000 0.2857143
#' #> 3    7 0.5000000 0.2857143 1.0000000
#' @importFrom igraph similarity V
#' @export get_jaccard_similarity

get_jaccard_similarity <- function(graph,
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

  # Get the Jaccard similarity scores
  if (direction == "all") {
    j_sim_values <-
      as.data.frame(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "all",
          method = "jaccard"))
  }

  if (direction == "out") {
    j_sim_values <-
      as.data.frame(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "out",
          method = "jaccard"))
  }

  if (direction == "in") {
    j_sim_values <-
      as.data.frame(
        igraph::similarity(
          ig_graph,
          vids = ig_nodes,
          mode = "in",
          method = "jaccard"))
  }

  # Create df with Jaccard similarity scores
  j_sim_values_df <-
    cbind(
      data.frame(node = get_node_df(graph)$nodes[nodes],
                 stringsAsFactors = FALSE),
      j_sim_values)

  # Modify the column names
  if (is.null(nodes)) {
    colnames(j_sim_values_df)[-1] <-
      paste0("n_", get_node_df(graph)$nodes)
  } else if (!is.null(nodes)) {
    colnames(j_sim_values_df)[-1] <-
      paste0("n_", get_node_df(graph)$nodes[nodes])
  }

  return(j_sim_values_df)
}
