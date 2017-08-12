#' Get the minimum cut between source and sink nodes
#' @description Get the minimum cut between source
#' and sink nodes. This is the minimum total capacity
#' of edges needed for removal in order to eliminate
#' all paths from the source and sink nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param from the node ID for the source node.
#' @param to the node ID for the sink or target node.
#' @return a single numeric value representing the
#' minimum total edge capacity removed to disconnect
#' the source and sink nodes.
#' @examples
#' # Set a seed
#' set.seed(23)
#'
#' # Create a cycle graph
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine the minimum cut
#' # between nodes `1` and `4`
#' get_min_cut_between(
#'   graph = graph,
#'   from = 1,
#'   to = 2)
#' #> [1] 1
#'
#' # Create a cycle graph with
#' # randomized values given to all
#' # edges as the `capacity` attribute
#' graph_capacity <-
#'   create_graph() %>%
#'   add_cycle(n = 5) %>%
#'   select_edges() %>%
#'   set_edge_attrs_ws(
#'     edge_attr = capacity,
#'     value =
#'       rnorm(
#'         n = edge_count(.),
#'         mean = 5,
#'         sd = 1)) %>%
#'   clear_selection()
#'
#' # Determine the minimum cut
#' # between nodes `1` and `4` for
#' # this graph, where `capacity`is
#' # set as an edge attribute
#' get_min_cut_between(
#'   graph = graph_capacity,
#'   from = 1,
#'   to = 2)
#' #> [1] 4.479822
#'
#' # Create a full graph and then
#' # get the minimum cut requirement
#' # between nodes `2` and `8`
#' create_graph() %>%
#'   add_full_graph(n = 10) %>%
#'   get_min_cut_between(
#'     from = 2,
#'     to = 8)
#' #> [1] 9
#' @importFrom igraph min_cut
#' @export get_min_cut_between

get_min_cut_between <- function(graph,
                                from,
                                to) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If the graph is empty, then return NA
  if (nrow(graph$nodes_df) == 0) {
    return(as.numeric(NA))
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the minimum cut value for removing
  # all paths from the source to the sink
  igraph::min_cut(
    graph = ig_graph,
    source = from,
    target = to)
}
