#' Determine whether a specified node is present in an
#' existing graph object
#' @description From a graph object of class
#' \code{dgr_graph}, determine whether a specified node
#' is present.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node a value that may or may not match a node
#' ID in the graph.
#' @return a logical value.
#' @examples
#' # Create a node data frame
#' nodes <-
#'   create_nodes(
#'     nodes = LETTERS,
#'     label = TRUE,
#'     type = "letter")
#'
#' # Create an edge data frame
#' edges <-
#'   create_edges(
#'     from = sample(LETTERS, replace = TRUE),
#'     to = sample(LETTERS, replace = TRUE),
#'     rel = "letter_to_letter")
#'
#' # Create a graph
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Verify that node with ID `a` is not in graph
#' node_present(graph, "a")
#' #> FALSE
#'
#' # Is node with ID `A` in the graph?
#' node_present(graph, "A")
#' #> TRUE
#'
#' # Are all node ID values from the LETTERS vector
#' # in the graph?
#' all(sapply(LETTERS,
#'       function(x) node_present(graph, x)))
#' #> TRUE
#' @export node_present

node_present <- function(graph,
                         node) {

  # Verify that `node` is given as a single value
  node_is_single_value <-
    ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE) {
    stop("Only a single node can be queried using 'node_present'")
  }

  # Determine whether the value corresponds to a
  # node ID in the graph
  if (node_is_single_value) {

    node_is_present <-
      ifelse(node %in% get_nodes(graph), TRUE, FALSE)

    return(node_is_present)
  }
}
