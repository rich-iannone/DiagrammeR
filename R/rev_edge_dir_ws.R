#' Reverse the direction of selected edges in a graph
#' @description Using a directed graph with a selection
#' of edges as input, reverse the direction of those
#' selected edges in input graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a
#' # directed tree
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 2)
#'
#' # Inspect the graph's edges
#' get_edges(graph)
#'
#' # Select all edges associated
#' # with nodes `1` and `2`
#' graph <-
#'   select_edges_by_node_id(
#'     graph = graph,
#'     nodes = 1:2)
#'
#' # Reverse the edge directions
#' # of the edges associated with
#' # nodes `1` and `2`
#' graph <-
#'   graph %>%
#'   rev_edge_dir_ws()
#'
#' # Inspect the graph's edges
#' # after their reversal
#' graph %>%
#'   get_edges()
#' @importFrom dplyr filter rename select everything bind_rows
#' @export rev_edge_dir_ws

rev_edge_dir_ws <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    stop(
      "The graph contains no edges, so, no edges can be reversed.",
      call. = FALSE)
  }

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {

    stop(
      "There is no selection of edges available.",
      call. = FALSE)
  }

  # If graph is undirected, stop function
  if (graph$directed == FALSE) {

    stop(
      "The input graph must be a directed graph.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  id <- from <- to <- . <- NULL

  # Get the graph nodes in the `from` and `to` columns
  # of the edf
  from <- get_edges(graph, return_type = "df")[, 1]
  to <- get_edges(graph, return_type = "df")[, 2]

  # Extract the graph's edge data frame
  edges <- get_edge_df(graph)

  # Get edge ID values in edge selection
  edge_ids <- get_selection(graph)

  # Selectively modify the edge direction and create
  # a new edf
  edges_new <-
    edges %>%
    dplyr::filter(id %in% edge_ids) %>%
    dplyr::filter(from != to) %>%
    dplyr::rename(from = to, to = from) %>%
    dplyr::select(id, from, to, dplyr::everything()) %>%
    dplyr::bind_rows(., edges %>% filter(!(id %in% edge_ids)))

  # Modify the graph object
  graph$edges_df <- edges_new

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "rev_edge_dir_ws",
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
