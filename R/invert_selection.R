#' Invert selection of nodes or edges in a graph
#' @description Modify the selection
#' of nodes or edges within a graph
#' object such that all nodes or edges
#' previously not selected will now be
#' selected and vice versa.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "standard")
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Select nodes with ID values `1` and `3`
#' graph <-
#'   select_nodes(
#'     graph = graph,
#'     nodes = c(1, 3))
#'
#' # Verify that a node selection has been made
#' get_selection(graph)
#' #> [1] 1 3
#'
#' # Invert the selection
#' graph <- invert_selection(graph)
#'
#' # Verify that the node selection has been changed
#' get_selection(graph)
#' #> [1] 2 4
#' @importFrom dplyr filter select
#' @export invert_selection

invert_selection <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph object has valid selection of
  # nodes or edges
  if (!(graph_contains_node_selection(graph) |
        graph_contains_edge_selection(graph))) {
    stop("There is no selection of nodes or edges available.")
  }

  # Create bindings for specific variables
  id <- from <- to <- NULL

  # Invert the nodes in the selection
  if (nrow(graph$node_selection) > 0) {

    selection_nodes <- graph$node_selection$node

    ndf <- graph$nodes_df

    inverted_nodes <-
      ndf %>%
      dplyr::filter(!(id %in% selection_nodes)) %>%
      dplyr::select(id)

    # Add the node ID values to the active selection
    # of nodes in `graph$node_selection`
    graph$node_selection <-
      replace_graph_node_selection(
        graph = graph,
        replacement = inverted_nodes$id)

    # Replace `graph$edge_selection` with an empty df
    graph$edge_selection <- create_empty_esdf()
  }

  # Invert the edges in the selection
  if (nrow(graph$edge_selection) > 0) {

    selection_edges <- graph$edge_selection$edge

    edf <- graph$edges_df

    inverted_edges <-
      edf %>%
      dplyr::filter(!(id %in% selection_edges)) %>%
      dplyr::select(id, from, to)

    # Add the node ID values to the active selection
    # of nodes in `graph$node_selection`
    graph$edge_selection <-
      replace_graph_edge_selection(
        graph = graph,
        edge_id = inverted_edges$id,
        from_node = inverted_edges$from,
        to_node = inverted_edges$to)

    # Replace `graph$node_selection` with an empty df
    graph$node_selection <- create_empty_nsdf()
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "invert_selection",
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
