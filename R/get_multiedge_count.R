#' Get the count of multiple edges in the graph
#' @description Get a count of the number of multiple
#' edges in the graph. Included in the count is the
#' number of separate edges that share the same edge
#' definition (i.e., same pair of nodes) across the
#' entire graph. So, for example, if there are 2
#' edge definitions in the graph that involve 6
#' separate edge IDs, the count will be \code{4}.
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
#' # Get the total number of multiple
#' # edges (those edges that share an
#' # edge definition) in the graph
#' get_multiedge_count(graph)
#' #> [1] 3
#' @importFrom dplyr distinct select
#' @export get_multiedge_count

get_multiedge_count <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no selections can be made.")
  }

  # Create bindings for specific variables
  from <- to <- NULL

  # Check for the number of multiple edges
  # regardless of which definitions these
  # edges have
  multiedge_count <-
    (graph$edges_df %>%
       nrow()) -
    (graph$edges_df %>%
       dplyr::select(from, to) %>%
       dplyr::distinct() %>%
       nrow())

  multiedge_count
}
