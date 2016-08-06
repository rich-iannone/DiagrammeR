#' Add one or more edges using a text string
#' @description With a graph object of class
#' \code{dgr_graph}, add one or more edges to the graph
#' using a text string.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edges a single-length vector with a character
#' string specifying the edges. For a directed graph,
#' the string object should be formatted as a series of
#' node ID values as \code{[node_ID_1]->[node_ID_2]}
#' separated by a one or more space characters. For
#' undirected graphs, \code{--} should replace
#' \code{->}. Linebreaks in the vector won't cause an
#' error.
#' @param rel an optional vector specifying the
#' relationship between the connected nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a graph with 10 nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10)
#'
#' # Add edges between nodes using a character string
#' graph <-
#'   graph %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6
#'      3->7 4->8 4->9 5->10")
#' @export add_edges_w_string

add_edges_w_string <- function(graph,
                               edges,
                               rel = NULL) {

  # Remove linebreak characters from `edges`
  edges_cleaned <-
    gsub("\n", " ", edges)

  # Remove extra spaces within the string
  edges_cleaned <-
    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
         edges_cleaned, perl = TRUE)

  # Split by single spaces into separate edge
  # expressions
  edges_split <-
    unlist(strsplit(edges_cleaned, " "))

  # Split the edge expressions in a directed
  # graph into `from` and `to` vectors
  if (graph$directed) {
    from <-
      sapply(strsplit(edges_split, "->"), "[[", 1)

    to <-
      sapply(strsplit(edges_split, "->"), "[[", 2)
  }

  # Split the edge expressions in an undirected
  # graph into `from` and `to` vectors
  if (graph$directed == FALSE) {
    from <-
      sapply(strsplit(edges_split, "--"), "[[", 1)

    to <-
      sapply(strsplit(edges_split, "--"), "[[", 2)
  }

  # Create an edge data frame (edf) without
  # associated `rel` values
  if (is.null(rel)) {
    new_edges <-
      create_edges(
        from = from,
        to = to)
  }

  # Create an edge data frame (edf) with
  # associated `rel` values
  if (!is.null(rel)) {
    new_edges <-
      create_edges(
        from = from,
        to = to,
        rel = rel)
  }

  # Add the new edges to the graph
  new_graph <-
    add_edge_df(graph,
                new_edges)

  return(new_graph)
}
