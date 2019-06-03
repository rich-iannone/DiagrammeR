#' Deselect any selected nodes in a graph
#'
#' Deselect nodes in a graph object of class `dgr_graph`.
#' @inheritParams render_graph
#' @param nodes a vector of node IDs that should be deselected.
#' @return a graph object of class `dgr_graph`.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "z", "z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("a", "z", "a"))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select nodes `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(nodes = c(1, 3)) %>%
#'   deselect_nodes(nodes = 1)
#'
#' # Verify that the node selection
#' # has been made for nodes `1` and
#' # `3` and that node `1` has been
#' # deselected (leaving only `3`)
#' graph %>% get_selection()
#' @importFrom dplyr filter
#' @export
deselect_nodes <- function(graph,
                           nodes) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If no node selection available, return graph
  if (graph_contains_node_selection(graph) == FALSE) {
    return(graph)
  }

  # Extract the graph's node selection
  graph$node_selection <-
    graph$node_selection %>%
    dplyr::filter(!(node %in% nodes))

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
