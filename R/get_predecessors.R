#' Get node IDs for predecessor nodes to the specified node
#'
#' Provides a vector of node IDs for all nodes that have a connection to the
#' given node.
#'
#' @inheritParams render_graph
#' @param node A node ID for the selected node.
#'
#' @return A vector of node ID values.
#'
#' @examples
#' # Set a seed
#' suppressWarnings(RNGversion("3.5.0"))
#' set.seed(23)
#'
#' # Create a node data frame (ndf)
#' ndf <- create_node_df(n = 26)
#'
#' # Create an edge data
#' # frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = sample(
#'       1:26, replace = TRUE),
#'     to = sample(
#'       1:26, replace = TRUE))
#'
#' # From the ndf and edf,
#' # create a graph object
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get predecessors for node
#' # `23` in the graph
#' graph %>%
#'   get_predecessors(
#'     node = 23)
#'
#' # If there are no predecessors,
#' # `NA` is returned
#' graph %>%
#'   get_predecessors(
#'     node = 26)
#'
#' @export
get_predecessors <- function(graph,
                             node) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If the graph contains no edges, return NA
  if (is.null(graph$edges_df)) {
    return(NA)
  }

  # Determine whether the graph has any nodes
  graph_is_not_empty <- !is_graph_empty(graph)

  # Determine whether `node` is in the graph
  node_is_in_graph <- node %in% graph$nodes_df$id

  # Obtain the node's predecessors
  if (graph_is_not_empty &
      node_is_in_graph &
      nrow(get_edge_info(graph)) > 0) {

    if (length(graph$edges_df[graph$edges_df$to ==
                              node,]$from) == 0) {
      predecessors <- NA
    } else {
      predecessors <-
        graph$edges_df[graph$edges_df$to == node,]$from
    }
    return(predecessors)
  }
}
