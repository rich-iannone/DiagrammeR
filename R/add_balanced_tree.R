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
#' @param nodes an optional vector of node IDs of
#' length \code{n} for the newly created nodes. If
#' nothing is provided, node IDs will assigned as
#' monotonically increasing integers.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
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
#' #> 2     8     8   tertiary   3     0      3     0
#' #> 3    21    21 quaternary   4     0      4     0
#' #> 4     2     2     binary   3     1      2     0
#' #> 5     3     3     binary   3     1      2     0
#' #> 6     9     9   tertiary   4     1      3     0
#' #> 7    10    10   tertiary   4     1      3     0
#' #> 8    11    11   tertiary   4     1      3     0
#' #> 9    22    22 quaternary   5     1      4     0
#' #> 10   23    23 quaternary   5     1      4     0
#' #> 11   24    24 quaternary   5     1      4     0
#' #> 12   25    25 quaternary   5     1      4     0
#' #> 13    4     4     binary   1     1      0     0
#' #> 14    5     5     binary   1     1      0     0
#' #> 15    6     6     binary   1     1      0     0
#' #> 16    7     7     binary   1     1      0     0
#' #> 17   12    12   tertiary   1     1      0     0
#' #> 18   13    13   tertiary   1     1      0     0
#' #> 19   14    14   tertiary   1     1      0     0
#' #> 20   15    15   tertiary   1     1      0     0
#' #> 21   16    16   tertiary   1     1      0     0
#' #> 22   17    17   tertiary   1     1      0     0
#' #> 23   18    18   tertiary   1     1      0     0
#' #> 24   19    19   tertiary   1     1      0     0
#' #> 25   20    20   tertiary   1     1      0     0
#' #> 26   26    26 quaternary   1     1      0     0
#' #> 27   27    27 quaternary   1     1      0     0
#' #> 28   28    28 quaternary   1     1      0     0
#' #> 29   29    29 quaternary   1     1      0     0
#' #> 30   30    30 quaternary   1     1      0     0
#' #> 31   31    31 quaternary   1     1      0     0
#' #> 32   32    32 quaternary   1     1      0     0
#' #> 33   33    33 quaternary   1     1      0     0
#' #> 34   34    34 quaternary   1     1      0     0
#' #> 35   35    35 quaternary   1     1      0     0
#' #> 36   36    36 quaternary   1     1      0     0
#' #> 37   37    37 quaternary   1     1      0     0
#' #> 38   38    38 quaternary   1     1      0     0
#' #> 39   39    39 quaternary   1     1      0     0
#' #> 40   40    40 quaternary   1     1      0     0
#' #> 41   41    41 quaternary   1     1      0     0
#' @export add_balanced_tree

add_balanced_tree <- function(graph,
                              k,
                              h,
                              type = NULL,
                              label = TRUE,
                              rel = NULL,
                              nodes = NULL){

  # Stop if k is too small
  if (k <= 1)  {
    stop("The value for k must be at least 2.")
  }

  # Stop if h is too small
  if (h <= 1)  {
    stop("The value for h must be at least 2.")
  }

  # Determine the number of nodes in the balanced tree
  n_nodes_tree <-
    (k^(h + 1) - 1) / (k - 1)

  if (!is.null(nodes)) {
    if (length(nodes) != n_nodes_tree) {
      stop("The number of node IDs supplied is not equal to number of nodes in the tree.")
    }

    if (any(get_nodes(graph) %in% nodes)) {
      stop("At least one of the node IDs is already present in the graph.")
    }
  }

  # If node IDs are not provided, create a
  # monotonically increasing ID value
  if (is.null(nodes)){

    if (node_count(graph) == 0){
      nodes <- seq(1, n_nodes_tree)
    }

    if (node_count(graph) > 0){
      if (!is.na(suppressWarnings(
        any(as.numeric(get_nodes(graph)))))){

        numeric_components <-
          suppressWarnings(which(!is.na(as.numeric(
            get_nodes(graph)))))

        nodes <-
          seq(max(
            as.integer(
              as.numeric(
                get_nodes(graph)[
                  numeric_components]))) + 1,
            max(
              as.integer(
                as.numeric(
                  get_nodes(graph)[
                    numeric_components]))) +
              n_nodes_tree)
      }

      if (suppressWarnings(all(is.na(as.numeric(
        get_nodes(graph)))))){
        nodes <- seq(1, n_nodes_tree)
      }
    }
  }

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

  graph <-
    add_edge_df(graph, tree_edges)

  return(graph)
}
