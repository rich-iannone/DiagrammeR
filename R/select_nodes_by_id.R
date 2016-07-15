#' Select nodes in a graph by ID values
#' @description Select nodes in a graph object of class
#' \code{dgr_graph} by their node ID values. If nodes
#' have IDs that are monotonically increasing integer
#' values, then numeric ranges can be used for the
#' selection.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param nodes a vector of node IDs for the selection
#' of nodes present in the graph.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a node data frame (ndf)
#' nodes <- create_nodes(nodes = 1:10)
#'
#' # Create a graph
#' graph <- create_graph(nodes_df = nodes)
#'
#' # Select nodes `1` to `5` and show that
#' # selection of nodes with `get_selection()`
#' graph %>%
#'   select_nodes_by_id(1:5) %>%
#'   get_selection
#' #> $nodes
#' #> [1] "1" "2" "3" "4" "5"
#' @export select_nodes_by_id

select_nodes_by_id <- function(graph,
                               nodes,
                               set_op = "union") {

  # Get a vector of node ID values from the graph
  nodes_in_graph <- graph$nodes_df$nodes

  # Ensure that the `nodes` vector is classed
  # as character
  nodes <- as.character(nodes)

  if (any(!(nodes %in% nodes_in_graph))) {
    stop("One of more of the nodes specified are not available in the graph.")
  }

  # Obtain vector of node IDs selection of nodes
  # already present in the graph
  if (!is.null(graph$selection)) {
    if (!is.null(graph$selection$nodes)) {
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected nodes into graph's
  # selection
  if (set_op == "union") {
    nodes_combined <-
      union(nodes_prev_selection, nodes)
  } else if (set_op == "intersect") {
    nodes_combined <-
      intersect(nodes_prev_selection, nodes)
  } else if (set_op == "difference") {
    nodes_combined <-
      setdiff(nodes_prev_selection, nodes)
  }

  graph$selection$nodes <- nodes_combined
  return(graph)
}
