#' Select last node in a series of node IDs in a graph
#' @description Select the last node from a graph
#' object of class \code{dgr_graph}. Strictly, this is
#' the node encompassing the last record of the graph's
#' node data frame. In practice, this will typically be
#' the last node added to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
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
#'   select_last_node()
#'
#' # Get the current selection
#' graph %>% get_selection()
#' #> [1] 3
#' @export select_last_node

select_last_node <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no node can be selected.")
  }

  nodes <- graph$nodes_df[, 1]
  last_node <- nodes[length(nodes)]
  graph$selection$nodes <- last_node

  if (!is.null(graph$selection$edges)) {
    graph$selection$edges <- NULL
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_last_node",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  return(graph)
}
