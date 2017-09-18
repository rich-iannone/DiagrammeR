#' Select edges in a graph using node ID values
#' @description Select edges in a graph object of class
#' \code{dgr_graph} using node ID values. All edges
#' associated with the provided nodes will be included
#' in the selection.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes a vector of node IDs for the selection
#' of edges present in the graph.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph edges This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 5 nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5)
#'
#' # Create a graph selection by selecting edges
#' # associated with nodes `1` and `2`
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(
#'     nodes = 1:2)
#'
#' # Get the selection of edges
#' graph %>%
#'   get_selection()
#' #> [1] 1 2
#'
#' # Perform another selection of edges, with nodes
#' # `1`, `2`, and `4`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges_by_node_id(
#'     nodes = c(1, 2, 4))
#'
#' # Get the selection of edges
#' graph %>%
#'   get_selection()
#' #> [1] 1 2 3 4
#'
#' # Get a fraction of the edges selected over all
#' # the edges in the graph
#' graph %>%
#' {
#'   l <- get_selection(.) %>%
#'     length(.)
#'   e <- count_edges(.)
#'   l/e
#' }
#' #> [1] 1
#' @importFrom dplyr filter select rename
#' @export select_edges_by_node_id

select_edges_by_node_id <- function(graph,
                                    nodes,
                                    set_op = "union") {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no selections can be made.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edges can be selected.")
  }

  # Create bindings for specific variables
  id <- from <- to <- NULL

  # Extract the graph's internal edf
  edges_df <- graph$edges_df

  edges_selected <-
    edges_df %>%
    dplyr::filter(from %in% nodes | to %in% nodes)

  # Create an integer vector representing edges
  edges_selected <- edges_selected$id

  # Obtain vector with node ID selection of edges
  # already present
  edges_prev_selection <- graph$edge_selection$edge

  # Incorporate the selected edges into the
  # graph's selection
  if (set_op == "union") {
    edges_combined <-
      union(edges_prev_selection, edges_selected)
  } else if (set_op == "intersect") {
    edges_combined <-
      intersect(edges_prev_selection, edges_selected)
  } else if (set_op == "difference") {
    edges_combined <-
      setdiff(edges_prev_selection, edges_selected)
  }

  # Filter `edges_df` to provide the correct esdf
  edges_combined <-
    graph$edges_df %>%
    dplyr::filter(id %in% edges_combined) %>%
    dplyr::select(id, from, to) %>%
    dplyr::rename(edge = id)

  # Add the edge ID values to the active selection
  # of edges in `graph$edge_selection`
  graph$edge_selection <- edges_combined

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_edges_by_node_id",
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
