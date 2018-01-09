#' Get the graph reciprocity
#' @description Get the reciprocity of a directed graph.
#' The reciprocity of a graph is the fraction of
#' reciprocal edges (e.g., `1` -> `2` and `2` -> `1`)
#' over all edges available in the graph. Note that for
#' an undirected graph, all edges are reciprocal. This
#' function does not consider loop edges (e.g., `1` -> `1`).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a single, numerical value that is the
#' ratio value of reciprocal edges over all graph
#' edges.
#' @examples
#' # Define a graph where 2 edge definitions
#' # have pairs of reciprocal edges
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 3) %>%
#'   add_node(
#'     from = 1,
#'       to = 1) %>%
#'   add_node(
#'     from = 1,
#'       to = 1)
#'
#' # Get the graph reciprocity, which will
#' # be calculated as the ratio 4/7 (where
#' # 4 is the number reciprocating edges
#' # and 7 is the total number of edges
#' # in the graph)
#' get_reciprocity(graph)
#' #> [1] 0.5714286
#'
#' # For an undirected graph, all edges
#' # are reciprocal, so the ratio will
#' # always be 1
#' graph %>%
#'   set_graph_undirected() %>%
#'   get_reciprocity()
#' #> [1] 1
#'
#' # For a graph with no edges, the graph
#' # reciprocity cannot be determined (and
#' # the same NA result is obtained from an
#' # empty graph)
#' create_graph() %>%
#'   add_n_nodes(n = 5) %>%
#'   get_reciprocity()
#' #> [1] NA
#' @importFrom igraph reciprocity
#' @export get_reciprocity

get_reciprocity <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # If the graph contains no edges, it
  # cannot return any valid reciprocity
  # value
  if (nrow(graph$edges_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the reciprocity value for the graph
  igraph::reciprocity(ig_graph)
}
