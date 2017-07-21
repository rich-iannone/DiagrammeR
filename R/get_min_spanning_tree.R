#' Get a minimum spanning tree subgraph
#' @description Get a minimum spanning tree subgraph
#' for a connected graph of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Obtain Jaccard similarity values for each
#' # pair of nodes as a square matrix
#' j_sim_matrix <-
#'   graph %>%
#'     get_jaccard_similarity()
#'
#' # Create a weighted, undirected graph from the
#' # resultant matrix (effectively treating that
#' # matrix as an adjacency matrix)
#' graph <-
#'   j_sim_matrix %>%
#'   from_adj_matrix(weighted = TRUE)
#'
#' # The graph in this case is a fully connected graph
#' # with loops, where jaccard similarity values are
#' # assigned as edge weights (edge attribute `weight`);
#' # The minimum spanning tree for this graph is the
#' # connected subgraph where the edges retained have
#' # the lowest similarity values possible
#' min_spanning_tree_graph <-
#'   graph %>%
#'   get_min_spanning_tree() %>%
#'   copy_edge_attrs(
#'     edge_attr_from = "weight",
#'     edge_attr_to = "label") %>%
#'   set_edge_attrs(
#'     edge_attr = "fontname",
#'     values = "Helvetica") %>%
#'   set_edge_attrs(
#'     edge_attr = "color",
#'     values = "gray85") %>%
#'   rescale_edge_attrs(
#'     edge_attr_from = "weight",
#'     to_lower_bound = 0.5,
#'     to_upper_bound = 4.0,
#'     edge_attr_to = "penwidth")
#' @importFrom igraph mst
#' @export get_min_spanning_tree

get_min_spanning_tree <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Transform the graph to an igraph object
  igraph <- to_igraph(graph)

  # Get the minimum spanning tree
  igraph_mst <- mst(igraph)

  # Generate the graph object from an igraph graph
  from_igraph(igraph_mst)
}
