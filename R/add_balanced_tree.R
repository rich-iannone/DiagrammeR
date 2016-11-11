#' Add a balanced tree of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a balanced tree to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param k the branching factor for the tree.
#' @param h the height of the tree.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node tree.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add 2 different types of
#' # balanced trees of height 2 (branching twice) and
#' # different branching ratios
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2, "binary") %>%
#'   add_balanced_tree(3, 2, "tertiary")
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>    id     type label deg indeg outdeg loops
#' #> 1   1   binary     1   2     0      2     0
#' #> 2   2   binary     2   3     1      2     0
#' #> 3   3   binary     3   3     1      2     0
#' #> 4   4   binary     4   1     1      0     0
#' #> 5   5   binary     5   1     1      0     0
#' #> 6   6   binary     6   1     1      0     0
#' #> .. ..      ...    ..  ..    ..     ..    ..
#' @export add_balanced_tree

add_balanced_tree <- function(graph,
                              k,
                              h,
                              type = NULL,
                              label = TRUE,
                              rel = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop if k is too small
  if (k <= 1) {
    stop("The value for k must be at least 2.")
  }

  # Stop if h is too small
  if (h <= 1) {
    stop("The value for h must be at least 2.")
  }

  # Determine the number of nodes in the balanced tree
  n_nodes_tree <-
    (k^(h + 1) - 1) / (k - 1)

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the sequence of nodes required
  nodes <- seq(1, n_nodes_tree)

  # Create a node data frame for the tree graph
  tree_nodes <-
    create_node_df(
      n = n_nodes_tree,
      type = type,
      label = label)

  # Create an edge data frame for the tree graph
  tree_edges <-
    create_edge_df(
      from = sort(
        rep(seq(nodes[1],
                nodes[length(
                  seq(nodes[2],
                      nodes[length(nodes)]))/k]), k)),
      to = seq(nodes[2], nodes[length(nodes)]),
      rel = rel)

  # Create the tree graph
  tree_graph <- create_graph(tree_nodes, tree_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    graph <- combine_graphs(graph, tree_graph)

    # Update the `last_node` counter
    graph$last_node <- nodes_created + nrow(tree_nodes)

    return(graph)
  } else {
    return(tree_graph)
  }
}
