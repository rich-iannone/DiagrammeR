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
#' # Create a new graph and add 3 different types of
#' # balanced trees of height 2 (branching twice) and
#' # different branching ratios
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2, "binary") %>%
#'   add_balanced_tree(3, 2, "tertiary") %>%
#'   add_balanced_tree(4, 2, "quaternary")
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>    node label       type deg indeg outdeg loops
#' #> 1     1     1     binary   2     0      2     0
#' #> 2    10    10   tertiary   4     1      3     0
#' #> 3    11    11   tertiary   4     1      3     0
#' #> 4    12    12   tertiary   1     1      0     0
#' #> 5    13    13   tertiary   1     1      0     0
#' #> 6    14    14   tertiary   1     1      0     0
#' #> ..   ..    .. ..........  ..    ..     ..    ..
#' @export add_balanced_tree

add_balanced_tree <- function(graph,
                              k,
                              h,
                              type = NULL,
                              label = TRUE,
                              rel = NULL) {

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

  nodes <-
    seq(nodes_created + 1,
        nodes_created + n_nodes_tree)

  tree_nodes <-
    create_nodes(
      nodes = nodes,
      type = type,
      label = label)

  graph <-
    add_node_df(graph, tree_nodes)

  tree_edges <-
    create_edges(
      from = sort(
        rep(seq(nodes[1],
                nodes[length(
                  seq(nodes[2],
                      nodes[length(nodes)]))/k]), k)),
      to = seq(nodes[2], nodes[length(nodes)]),
      rel = rel)

  graph <- add_edge_df(graph, tree_edges)

  # Update the `last_node` counter
  graph$last_node <- nodes_created + nrow(tree_nodes)

  return(graph)
}
