#' Deselect any selected edges in a graph
#' @description Deselect edges in a graph object of
#' class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edges a vector of edge IDs that
#' should be deselected.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with
#' # a single path
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Select edges with IDs `1`
#' # and `3`
#' graph <-
#'   graph %>%
#'   select_edges_by_edge_id(
#'     edges = c(1, 3))
#'
#' # Verify that an edge selection
#' # has been made
#' graph %>%
#'   get_selection()
#'
#' # Deselect edge `1`
#' graph <-
#'   graph %>%
#'   select_edges_by_edge_id(
#'     edges = c(1, 3)) %>%
#'   deselect_edges(edges = 1)
#'
#' # Verify that the edge selection
#' # has been made for edges `1` and
#' # `3` and that edge `1` has been
#' # deselected (leaving only `3`)
#' graph %>%
#'   get_selection()
#' @importFrom dplyr filter
#' @export deselect_edges

deselect_edges <- function(graph,
                           edges) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # If no edge selection available, return graph
  if (graph_contains_edge_selection(graph) == FALSE) {
    return(graph)
  }

  # Create binding for specific variable
  edge <- NULL

  # Extract the graph's edge selection
  graph$edge_selection <-
    graph$edge_selection %>%
    dplyr::filter(!(edge %in% edges))

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "deselect_edges",
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
