#' Get count of edge definitions where multiple edges occur
#' @description Get a count of the number of edge
#' definitions (e.g, `1` -> `2`) where there are multiple
#' edges (i.e., more than 1 edge of that definition, having
#' distinct edge ID values). So, for example, if there are 2
#' edge definitions in the graph that involve 6 separate
#' edge IDs (3 such edge IDs for each of the pairs of
#' nodes), the count will be \code{2}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector with a single, numerical value.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 5,
#'     label = TRUE)
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 4, 4, 3, 5, 1, 3, 4),
#'       to = c(4, 1, 1, 2, 2, 2, 2, 1))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get the total number of edge
#' # definitions (e.g., `4` -> `1`) where
#' # there are multiple edges (i.e.,
#' # distinct edges with separate edge
#' # ID values)
#' get_edge_count_w_multiedge(graph)
#' @importFrom dplyr select mutate group_by summarize ungroup filter n
#' @export get_edge_count_w_multiedge

get_edge_count_w_multiedge <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    stop(
      "The graph contains no edges, so, no selections can be made.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  from <- to <- edge_from_to <- NULL

  # Check for the number of multiple edges
  # regardless of which definitions these
  # edges have
  multiedge_distinct_edge_def_count <-
    graph$edges_df %>%
    dplyr::select(from, to) %>%
    dplyr::mutate(edge_from_to = paste0(from, "_", to)) %>%
    dplyr::select(edge_from_to) %>%
    dplyr::group_by(edge_from_to) %>%
    dplyr::summarize(n = n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n > 1) %>%
    nrow()

  multiedge_distinct_edge_def_count
}
