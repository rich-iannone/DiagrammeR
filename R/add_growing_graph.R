#' Create a random growing graph with m edges added per step
#' @description To an existing graph
#' object, add a graph built by adding
#' \code{m} new edges at each time
#' step (where a node is added).
#' @param graph a graph object of
#' class \code{dgr_graph}.
#' @param n the number of nodes
#' comprising the generated graph.
#' @param m the number of edges
#' added per time step.
#' @param citation a logical value
#' (default is \code{FALSE}) that
#' governs whether a citation graph
#' is to be created. This is where
#' new edges specifically originate
#' from the newly added node in the
#' most recent time step.
#' @param set_seed supplying a
#' value sets a random seed of the
#' \code{Mersenne-Twister}
#' implementation.
#' @param node_aes an optional list
#' of named vectors comprising node
#' aesthetic attributes. The helper
#' function \code{node_aes()} is
#' strongly recommended for use here
#' as it contains arguments for each
#' of the accepted node aesthetic
#' attributes (e.g., \code{shape},
#' \code{style}, \code{color},
#' \code{fillcolor}).
#' @param edge_aes an optional list
#' of named vectors comprising edge
#' aesthetic attributes. The helper
#' function \code{edge_aes()} is
#' strongly recommended for use here
#' as it contains arguments for each
#' of the accepted edge aesthetic
#' attributes (e.g., \code{shape},
#' \code{style}, \code{penwidth},
#' \code{color}).
#' @param node_data an optional list
#' of named vectors comprising node
#' data attributes. The helper
#' function \code{node_data()} is
#' strongly recommended for use here
#' as it helps bind data specifically
#' to the created nodes.
#' @param edge_data an optional list
#' of named vectors comprising edge
#' data attributes. The helper function
#' \code{edge_data()} is strongly
#' recommended for use here as it helps
#' bind data specifically to the
#' created edges.
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
#' growing_graph %>%
#'   count_nodes()
#' #> [1] 100
#'
#' # Get a count of edges
#' growing_graph %>%
#'   count_edges()
#' #> [1] 99
#' @importFrom igraph sample_growing
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @export add_growing_graph

add_growing_graph <- function(graph,
                              n,
                              m = 1,
                              citation = FALSE,
                              set_seed = NULL,
                              node_aes = NULL,
                              edge_aes = NULL,
                              node_data = NULL,
                              edge_data = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  index__ <- id <- NULL

  # If a seed value is supplied, set a seed
  if (!is.null(set_seed)) {
    set.seed(set_seed, kind = "Mersenne-Twister")
  }

  # Stop if n is too small
  if (n <= 0)  {
    stop("The value for `n` must be at least 1.")
  }

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

  n_nodes <- nrow(sample_growing_graph$nodes_df)

  n_edges <- nrow(sample_growing_graph$edges_df)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- tibble::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < nrow(sample_growing_graph$nodes_df)) {

      node_aes$index__ <- 1:nrow(sample_growing_graph$nodes_df)

      node_aes_tbl <-
        tibble::as_tibble(node_aes) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(node_aes_tbl)) {
      node_aes_tbl <-
        node_aes_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect node data attributes
  if (!is.null(node_data)) {

    node_data_tbl <- tibble::as_tibble(node_data)

    if (nrow(node_data_tbl) < nrow(sample_growing_graph$nodes_df)) {

      node_data$index__ <- 1:nrow(sample_growing_graph$nodes_df)

      node_data_tbl <-
        tibble::as_tibble(node_data) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(node_data_tbl)) {
      node_data_tbl <-
        node_data_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- tibble::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < nrow(sample_growing_graph$edges_df)) {

      edge_aes$index__ <- 1:nrow(sample_growing_graph$edges_df)

      edge_aes_tbl <-
        tibble::as_tibble(edge_aes) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(edge_aes_tbl)) {
      edge_aes_tbl <-
        edge_aes_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- tibble::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < nrow(sample_growing_graph$edges_df)) {

      edge_data$index__ <- 1:nrow(sample_growing_graph$edges_df)

      edge_data_tbl <-
        tibble::as_tibble(edge_data) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(edge_data_tbl)) {
      edge_data_tbl <-
        edge_data_tbl %>%
        dplyr::select(-id)
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
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, sample_growing_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(n_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(n_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_growing_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df),
        d_n = n_nodes,
        d_e = n_edges)

    combined_graph$global_attrs <- global_attrs
    combined_graph$graph_log <- graph_log
    combined_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(combined_graph$graph_actions) > 0) {
      combined_graph <-
        combined_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (combined_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = combined_graph)
    }

    return(combined_graph)

  } else {

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_growing_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(sample_growing_graph$nodes_df),
        edges = nrow(sample_growing_graph$edges_df),
        d_n = n_nodes,
        d_e = n_edges)

    sample_growing_graph$global_attrs <- global_attrs
    sample_growing_graph$graph_log <- graph_log
    sample_growing_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(sample_growing_graph$graph_actions) > 0) {
      sample_growing_graph <-
        sample_growing_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (sample_growing_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = sample_growing_graph)
    }

    return(sample_growing_graph)
  }
}
