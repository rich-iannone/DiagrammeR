#' Select last edge in a series of edges defined in a
#' graph
#' @description Select the last edge from a graph
#' object of class \code{dgr_graph}. Strictly, this is
#' the edge definition that encompasses the last record
#' of the graph's edge data frame. In practice, this
#' will typically be the last edge created.
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
#' # Add two edges to the graph
#' graph %<>%
#'   add_edge(1, 2) %>%
#'   add_edge(2, 3)
#'
#' # Select the last edge added
#' graph <-
#'   graph %>%
#'   select_last_edge()
#'
#' # Get the current selection
#' graph %>% get_selection()
#' #> [1] 2
#' @importFrom dplyr select rename
#' @export select_last_edge

select_last_edge <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edge can be selected.")
  }

  # Create bindings for specific variables
  id <- from <- to <- NULL

  # Extract the graph's internal edf
  edges_df <- graph$edges_df

  # Get the last edge created
  last_edge <-
    edges_df[nrow(edges_df), ] %>%
    dplyr::select(id, from, to) %>%
    dplyr::rename(edge = id)

  # Set the edge ID value as the active selection
  # of edges in `graph$edge_selection`
  graph$edge_selection <- last_edge

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_last_edge",
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
