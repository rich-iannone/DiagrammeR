#' Traverse from one or more selected edges toward
#' adjacent outward nodes
#' @description From a graph object of class
#' \code{dgr_graph} move to adjacent nodes from a
#' selection of one or more selected edges where the
#' edges are outward edges to those nodes. This creates
#' a selection of nodes. An optional filter by node
#' attribute can limit the set of nodes traversed to.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr an optional character vector of
#' node attribute values for filtering the node ID
#' values returned.
#' @param match an option to provide a logical
#' expression with a comparison operator (\code{>},
#' \code{<}, \code{==}, or \code{!=}) followed by a
#' number for numerical filtering, or, a character
#' string for filtering the edges returned through
#' string matching.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a simple graph
#' graph <-
#'  create_graph() %>%
#'  add_n_nodes(2) %>%
#'  add_edge(1, 2)
#'
#' # Traverse from node `1` to the outward edge and
#' # immediately back to `1` by:
#' # (1) moving from node `1` to edge `1` -> `2`
#' # (2) moving from edge `1` -> `2` to node `1`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge %>%
#'   trav_out_node
#'
#' # Verify that the selection of node `1` has been
#' # made by using the `get_selection()` function
#' get_selection(graph)
#' #> [1] "1"
#' @export trav_out_node

trav_out_node <- function(graph,
                          node_attr = NULL,
                          match = NULL) {

  if (is.null(graph$selection$edges)) {
    stop("There is no selection of edges available.")
  }

  # Create a selection of nodes using the
  # edge selection
  landing_nodes <- unique(graph$selection$edges$from)

  # If a match term provided, filter using a
  # logical expression or a regex match
  if (!is.null(match)) {

    if (grepl("^>.*", match) |
        grepl("^<.*", match) |
        grepl("^==.*", match) |
        grepl("^!=.*", match)) {
      logical_expression <- TRUE } else {
        logical_expression <- FALSE
      }

    if (logical_expression) {

      for (i in 1:length(landing_nodes)) {

        if (i == 1) {
          to_nodes <- vector(mode = "character")

          column_number <-
            which(colnames(graph$nodes_df) %in%
                    node_attr)
        }

        if (grepl("^>.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    landing_nodes[i]),
              column_number]) >
            as.numeric(gsub(">(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }

        if (grepl("^<.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    landing_nodes[i]),
              column_number]) <
            as.numeric(gsub("<(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }

        if (grepl("^==.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    landing_nodes[i]),
              column_number]) ==
            as.numeric(gsub("==(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }

        if (grepl("^!=.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    landing_nodes[i]),
              column_number]) !=
            as.numeric(gsub("!=(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, landing_nodes[i])
          }
        }
      }
    }

    # Filter using a `match` value
    if (logical_expression == FALSE) {

      if (is.numeric(match)) {
        match <- as.character(match)
      }

      for (i in 1:length(landing_nodes)) {

        if (i == 1) {
          to_nodes <- vector(mode = "character")
          column_number <-
            which(colnames(graph$nodes_df) %in%
                    node_attr)
        }

        if (match ==
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    landing_nodes[i]),
              column_number]) {

          to_nodes <- c(to_nodes, landing_nodes[i])
        }
      }
    }

    landing_nodes <- to_nodes
  }

  # If there are no valid traversals, return the same
  # graph object without modifying the node selection
  if (length(landing_nodes) == 0) {
    return(graph)
  }

  # Remove the edge selection in graph
  graph$selection$edges <- NULL

  # Update node selection in graph
  graph$selection$nodes <- landing_nodes

  return(graph)
}
