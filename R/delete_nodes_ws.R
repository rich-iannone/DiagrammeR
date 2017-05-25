#' Delete all selected nodes in a node selection
#' @description In a graph object of class
#' \code{dgr_graph}, delete all nodes present in a
#' selection.
#'
#' Selections of nodes can be performed using
#' the following \code{select_...} functions:
#' \code{select_nodes()},
#' \code{select_last_nodes_created()},
#' \code{select_nodes_by_degree()},
#' \code{select_nodes_by_id()}, or
#' \code{select_nodes_in_neighborhood()}.
#' Selections of nodes can also be performed using
#' the following traversal functions:
#' (\code{trav_...}):
#' \code{trav_out()}, \code{trav_in()},
#' \code{trav_both()}, \code{trav_in_node()},
#' \code{trav_out_node()}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 3 nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 3) %>%
#'   add_edges_w_string(
#'     edges = "1->3 1->2 2->3")
#'
#' # Select node with ID `1`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(nodes = 1)
#'
#' # Delete node in selection (this
#' # also deletes any attached edges)
#' graph <-
#'   graph %>%
#'   delete_nodes_ws()
#'
#' # Get a count of nodes in the graph
#' node_count(graph)
#' #> [1] 2
#' @export delete_nodes_ws

delete_nodes_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no nodes can be deleted.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes available.")
  }

  # Get a vector of the nodes to be deleted
  nodes_to_delete <- graph$node_selection$node

  # Delete all nodes in `nodes_to_delete`
  for (i in 1:length(nodes_to_delete)) {

    graph <-
      delete_node(
        graph = graph,
        node = nodes_to_delete[i])
  }

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Remove any `delete_node` records from the graph log
  graph$graph_log <-
    graph$graph_log[-((nrow(graph$graph_log) - (i-1)):nrow(graph$graph_log)), ]

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "delete_nodes_ws",
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
