#' Select last node in a series of node IDs in a graph
#' @description Select the last node from a graph
#' object of class \code{dgr_graph}. Strictly, this is
#' the node encompassing the last record of the graph's
#' node data frame. In practice, this will typically be
#' the last node added to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Add three nodes to the graph
#' graph <-
#'   graph %>%
#'   add_n_nodes(3)
#'
#' # Select the last node added
#' graph <-
#'   graph %>%
#'   select_last_node
#'
#' # Get the current selection
#' graph %>% get_selection
#' #> [1] 3
#' @export select_last_node

select_last_node <- function(graph) {

  if (is_graph_empty(graph)) {
    stop("The graph is empty so no selections can be made.")
  }

  nodes <- graph$nodes_df[, 1]
  last_node <- nodes[length(nodes)]
  graph$selection$nodes <- last_node

  if (!is.null(graph$selection$edges)) {
    graph$selection$edges <- NULL
  }

  return(graph)
}
