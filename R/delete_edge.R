#' Delete an edge from an existing graph object
#' @description From a graph object of class
#' \code{dgr_graph}, delete an existing edge by
#' specifying either: (1) a pair of node IDs
#' corresponding to the edge (keeping into
#' consideration the direction of the edge in
#' a directed graph), or (2) an edge ID.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param from a node ID from which the edge to be
#' removed is outgoing.
#' @param to a node ID to which the edge to be removed
#' is incoming.
#' @param id an edge ID of an edge to be removed.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Add two nodes
#' graph <- add_node(graph)
#' graph <- add_node(graph)
#'
#' # Add an edge
#' graph <-
#'   add_edge(
#'     graph = graph,
#'     from = 1,
#'     to = 2)
#'
#' # Delete the edge
#' graph <-
#'   delete_edge(
#'     graph = graph,
#'     from = 1,
#'     to = 2)
#'
#' # Get the count of edges in the graph
#' edge_count(graph)
#' #> [1] 0
#'
#' # Create an undirected graph with
#' # 2 nodes and an edge
#' graph_undirected <-
#'   create_graph(directed = FALSE) %>%
#'   add_n_nodes(2) %>%
#'   add_edge(1, 2)
#'
#' # Delete the edge; order of node ID
#' # values provided in `from` and `to`
#' # don't matter for the undirected case
#' graph_undirected %>%
#'   delete_edge(2, 1) %>%
#'   edge_count()
#' #> [1] 0
#'
#' graph_undirected %>%
#'   delete_edge(1, 2) %>%
#'   edge_count()
#' #> [1] 0
#' @importFrom dplyr filter
#' @export delete_edge

delete_edge <- function(graph,
                        from = NULL,
                        to = NULL,
                        id = NULL) {

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
    stop("The graph contains no edges, so, no selections can be made.")
  }

  # If a value is supplied for `id`, determine which
  # node ID values the edge ID references
  if (!is.null(id)) {

    # Stop function if the edge ID value supplied is
    # not in the graph's edf
    if (!(id %in% graph$edges_df$id)) {
      stop("The edge specified is not present in the graph.")
    }

    # Get the node ID values for the edge ID
    from_id <- graph$edges_df[which(graph$edges_df$id == id)[1], 2]
    to_id <- graph$edges_df[which(graph$edges_df$id == id)[1], 3]

  } else if (is.null(id)) {

    # Verify that each of the values for `from` and
    # `to` are given as single values
    from_is_single_value <-
      ifelse(length(from) == 1, TRUE, FALSE)

    to_is_single_value <-
      ifelse(length(to) == 1, TRUE, FALSE)

    # Stop function if either node is not a single value
    if (from_is_single_value == FALSE |
        to_is_single_value == FALSE) {
      stop("Single-length vectors for `from` and `to` should be specified.")
    }

    # Change variable names
    from_id <- from
    to_id <- to
  }

  # Determine whether the pair of nodes provided
  # are in the graph
  if (from_is_single_value &
      to_is_single_value) {
    nodes_available_in_graph <-
      ifelse(all(c(from_id, to_id) %in%
                   get_node_ids(graph)),
             TRUE, FALSE)
  }

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE) {
    stop("The nodes specified are not both present in the graph.")
  }

  # Extract the graph's edge data frame
  edf <- graph$edges_df

  # Edge removal case for directed graphs
  if (is_graph_directed(graph)) {

    # Stop function if the edge provided is not
    # in the edge data frame
    if (edf %>%
        dplyr::filter(from == from_id & to == to_id) %>%
        nrow == 0) {
      stop("The edge provided is not in the graph.")
    }

    # Filter out relevant rows from `edf`
    edf <-
      edf %>%
      dplyr::filter(!(from == from_id & to == to_id))

    # Reset the row names in the edf
    row.names(edf) <- NULL

    # Update the graph's edge data frame
    graph$edges_df <- edf
  }

  # Edge removal case for undirected graphs
  if (is_graph_directed(graph) == FALSE) {

    # Stop function if the edge provided is not
    # in the edge data frame
    if (edf %>%
        dplyr::filter((from == from_id & to == to_id) |
                      (from == to_id & to == from_id)) %>%
        nrow == 0) {
      stop("The edge provided is not in the graph.")
    }

    # Filter out relevant rows from `edf`
    edf <-
      edf %>%
      dplyr::filter(!((from == from_id & to == to_id) |
                        (from == to_id & to == from_id)))

    row.names(edf) <- NULL

    # Update the graph's edge data frame
    graph$edges_df <- edf
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "delete_edge",
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
