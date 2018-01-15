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
#' removed is outgoing. If an edge ID is provided to
#' \code{id}, then this argument is ignored. There
#' is the option to use a node \code{label}
#' value here (and this must correspondingly also be
#' done for the \code{to} argument) for defining
#' node connections. Note that this is only possible
#' if all nodes have distinct \code{label} values set
#' and none exist as an empty string.
#' @param to a node ID to which the edge to be removed
#' is incoming. If an edge ID is provided to
#' \code{id}, then this argument is ignored. There
#' is the option to use a node \code{label}
#' value here (and this must correspondingly also be
#' for the \code{from} argument) for defining
#' node connections. Note that this is only possible
#' if all nodes have distinct \code{label} values set
#' and none exist as an empty string.
#' @param id an edge ID of the edge to be removed.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 2 nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 2)
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
#' count_edges(graph)
#'
#' # Create an undirected graph with
#' # 2 nodes and an edge
#' graph_undirected <-
#'   create_graph(directed = FALSE) %>%
#'   add_n_nodes(n = 2) %>%
#'   add_edge(
#'     from = 1,
#'     to = 2)
#'
#' # Delete the edge; the order of node ID
#' # values provided in `from` and `to`
#' # don't matter for the undirected case
#' graph_undirected %>%
#'   delete_edge(
#'     from = 2,
#'     to = 1) %>%
#'   count_edges()
#'
#' graph_undirected %>%
#'   delete_edge(
#'     from = 1,
#'     to = 2) %>%
#'   count_edges()
#'
#' # The undirected graph has a single
#' # edge with ID `1`; it can be
#' # deleted by specifying `id`
#' graph_undirected %>%
#'   delete_edge(id = 1) %>%
#'   count_edges()
#'
#' # Create a directed graph with 2
#' # labeled nodes and an edge
#' graph_labeled_nodes <-
#'   create_graph() %>%
#'   add_n_nodes(
#'     n = 2,
#'     label = c("one", "two")) %>%
#'   add_edge(
#'     from = "one",
#'     to = "two")
#'
#' # Delete the edge using the node
#' # labels in `from` and `to`; this
#' # is analogous to creating the
#' # edge using node labels
#' graph_labeled_nodes %>%
#'   delete_edge(
#'     from = "one",
#'     to = "two") %>%
#'   count_edges()
#' @importFrom dplyr filter select
#' @export delete_edge

delete_edge <- function(graph,
                        from = NULL,
                        to = NULL,
                        id = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, no selections can be made.",
      call. = FALSE)
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    stop(
      "The graph contains no edges, so, no selections can be made.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  label <- NULL

  # If a value is supplied for `id`, determine which
  # node ID values the edge ID references
  if (!is.null(id)) {

    # Stop function if the edge ID value supplied is
    # not in the graph's edf
    if (!(id %in% graph$edges_df$id)) {

      stop(
        "The edge specified is not present in the graph.",
        call. = FALSE)
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

      stop(
        "Single-length vectors for `from` and `to` should be specified.",
        call. = FALSE)
    }

    # If `from` and `to` values provided as character
    # values, assume that these values refer to node
    # `label` attr values
    if (is.character(from) & is.character(to)) {

      # Stop function if the label for `from` exists in the graph
      if (!(from %in% graph$nodes_df$label)) {

        stop(
          "The value provided in `from` does not exist as a node `label` value.",
          call. = FALSE)
      }

      # Stop function if the label for `from` is not distinct in the graph
      if (graph$nodes_df %>%
          dplyr::select(label) %>%
          dplyr::filter(label == from) %>%
          nrow() > 1) {

        stop(
          "The node `label` provided in `from` is not distinct in the graph.",
          call. = FALSE)
      }

      # Stop function if the label for `to` exists in the graph
      if (!(to %in% graph$nodes_df$label)) {

        stop(
          "The value provided in `to` does not exist as a node `label` value.",
          call. = FALSE)
      }

      # Stop function if the label for `to` is not distinct in the graph
      if (graph$nodes_df %>%
          dplyr::select(label) %>%
          dplyr::filter(label == to) %>%
          nrow() > 1) {

        stop(
          "The node `label` provided in `to` is not distinct in the graph.",
          call. = FALSE)
      }

      # Use the `translate_to_node_id()` helper function to map
      # node `label` values to node `id` values
      from_to_node_id <-
        translate_to_node_id(
          graph = graph,
          from = from,
          to = to)

      from_id <- from_to_node_id$from
      to_id <- from_to_node_id$to

    } else {

      from_id <- from
      to_id <- to
    }
  }

  # Determine whether the pair of nodes provided
  # are in the graph
  nodes_available_in_graph <-
    ifelse(
      all(c(from_id, to_id) %in%
            get_node_ids(graph)), TRUE, FALSE)

  # Stop function if both nodes not present in graph
  if (nodes_available_in_graph == FALSE) {

    stop(
      "The nodes specified are not both present in the graph.",
      call. = FALSE)
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

      stop(
        "The edge provided is not in the graph.",
        call. = FALSE)
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

      stop(
        "The edge provided is not in the graph.",
        call. = FALSE)
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

  # Scavenge any invalid, linked data frames
  graph <-
    graph %>%
    remove_linked_dfs()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "delete_edge",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_e = as.integer(-1))

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
