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

  # Create bindings for specific variables
  id <- NULL

  # Extract the graph's internal ndf
  nodes_df <- graph$nodes_df

  # Get the last node created
  last_node <-
    nodes_df[nrow(nodes_df), ] %>%
    dplyr::select(id) %>%
    dplyr::rename(node = id)

  # Set the node ID value as the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <- last_node

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

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

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
