#' Create a random growing graph with m edges added per step
#'
#' @description
#'
#' To an existing graph object, add a graph built by adding `m` new edges at
#' each time step (where a node is added).
#'
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param n The number of nodes comprising the generated graph.
#' @param m The number of edges added per time step.
#' @param citation A logical value (default is `FALSE`) that governs whether a
#'   citation graph is to be created. This is where new edges specifically
#'   originate from the newly added node in the most recent time step.
#' @param type An optional string that describes the entity type for all the
#'   nodes to be added.
#' @param label A logical value where setting to `TRUE` ascribes node IDs to the
#'   label and `FALSE` yields a blank label.
#' @param rel An optional string for providing a relationship label to all edges
#'   to be added.
#' @param set_seed Supplying a value sets a random seed of the
#'   `Mersenne-Twister` implementation.
#' @examples
#' # Create a random, growing
#' # citation graph with 100
#' # nodes, adding an edge after
#' # each node addition
#' growing_graph <-
#'   create_graph() %>%
#'   add_growing_graph(
#'     n = 100,
#'     m = 1,
#'     citation = TRUE,
#'     set_seed = 23)
#'
#' # Get a count of nodes
#' growing_graph %>% count_nodes()
#'
#' # Get a count of edges
#' growing_graph %>% count_edges()
#'
#' @export
add_growing_graph <- function(
    graph,
    n,
    m = 1,
    citation = FALSE,
    type = NULL,
    label = TRUE,
    rel = NULL,
    node_aes = NULL,
    edge_aes = NULL,
    node_data = NULL,
    edge_data = NULL,
    set_seed = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If a seed value is supplied, set a seed
  if (!is.null(set_seed)) {
    set.seed(set_seed, kind = "Mersenne-Twister")
  }

  # Stop if n is too small
  check_number_whole(n, min = 1)

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the number of edges ever created for
  # this graph
  edges_created <- graph$last_edge

  # Get the graph's global attributes
  global_attrs <- graph$global_attrs

  # Get the graph's log
  graph_log <- graph$graph_log

  # Get the graph's info
  graph_info <- graph$graph_info

  # Get the graph's state of being directed
  # or undirected
  graph_directed <- graph$directed

  # Create the graph to be added
  sample_growing_igraph <-
    igraph::sample_growing(
      n = n,
      m = m,
      directed = graph_directed,
      citation = citation)

  sample_growing_graph <- from_igraph(sample_growing_igraph)

  # Add in a static `type` value for all new nodes
  if (!is.null(type)) {
    sample_growing_graph$nodes_df$type <- as.character(type[1])
  }

  # Add in a static `rel` value for all new nodes
  if (!is.null(rel)) {
    sample_growing_graph$edges_df$rel <- as.character(rel[1])
  }

  # If `label` is requested, use the node ID to
  # create a unique label for all new nodes
  if (label) {
    sample_growing_graph$nodes_df$label <-
      sample_growing_graph$nodes_df$id %>% as.character()
  }

  n_nodes <- nrow(sample_growing_graph$nodes_df)

  n_edges <- nrow(sample_growing_graph$edges_df)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < nrow(sample_growing_graph$nodes_df)) {

      node_aes$index__ <- seq_len(nrow(sample_growing_graph$nodes_df))

      node_aes_tbl <-
        dplyr::as_tibble(node_aes) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(node_aes_tbl)) {
      node_aes_tbl$id <- NULL
    }
  }

  # Collect node data attributes
  if (!is.null(node_data)) {

    node_data_tbl <- dplyr::as_tibble(node_data)

    if (nrow(node_data_tbl) < nrow(sample_growing_graph$nodes_df)) {

      node_data$index__ <- seq_len(nrow(sample_growing_graph$nodes_df))

      node_data_tbl <-
        dplyr::as_tibble(node_data) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(node_data_tbl)) {
      node_data_tbl$id <- NULL
    }
  }

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- dplyr::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < nrow(sample_growing_graph$edges_df)) {

      edge_aes$index__ <- seq_len(nrow(sample_growing_graph$edges_df))

      edge_aes_tbl <-
        dplyr::as_tibble(edge_aes) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(edge_aes_tbl)) {
      edge_aes_tbl$id <- NULL
    }
  }

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- dplyr::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < nrow(sample_growing_graph$edges_df)) {

      edge_data$index__ <- seq_len(nrow(sample_growing_graph$edges_df))

      edge_data_tbl <-
        dplyr::as_tibble(edge_data) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(edge_data_tbl)) {
      edge_data_tbl$id <- NULL
    }
  }

  # Add node aesthetics if available
  if (exists("node_aes_tbl")) {

    sample_growing_graph$nodes_df <-
      sample_growing_graph$nodes_df %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    sample_growing_graph$nodes_df <-
      sample_growing_graph$nodes_df %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    sample_growing_graph$edges_df <-
      sample_growing_graph$edges_df %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    sample_growing_graph$edges_df <-
      sample_growing_graph$edges_df %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (is_graph_empty(graph)) {
    graph <- sample_growing_graph
  } else {
    graph <- combine_graphs(graph, sample_growing_graph)
  }

  # Update the `last_node` counter
  graph$last_node <- nodes_created + n_nodes

  # Update the `last_edge` counter
  graph$last_edge <- edges_created + n_edges

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph_log <-
    add_action_to_log(
      graph_log = graph_log,
      version_id = nrow(graph_log) + 1L,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = n_nodes,
      d_e = n_edges)

  graph$global_attrs <- global_attrs
  graph$graph_log <- graph_log
  graph$graph_info <- graph_info

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      trigger_graph_actions(graph)
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
