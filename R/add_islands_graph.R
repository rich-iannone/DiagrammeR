#' Create a random islands graph with edges between the islands
#' @description To an existing graph
#' object, add several Erdos-Renyi
#' random graphs (the islands) using
#' a common set of parameters, connected
#' together by a fixed number of edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n_islands the number of
#' islands in the generated graph.
#' @param island_size the size of
#' the islands in the generated graph.
#' @param p the probability of there
#' being edges between the islands.
#' @param edges_between The number
#' of edges between islands.
#' @param type an optional string that
#' describes the entity type for all the
#' nodes to be added.
#' @param label a boolean value where
#' setting to \code{TRUE} ascribes node
#' IDs to the label and \code{FALSE}
#' yields a blank label.
#' @param rel an optional string for
#' providing a relationship label to all
#' edges to be added.
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
#' @param set_seed supplying a value
#' sets a random seed of the
#' \code{Mersenne-Twister}
#' implementation.
#' @examples
#' # Create a graph of islands
#' islands_graph <-
#'   create_graph() %>%
#'   add_islands_graph(
#'     n_islands = 4,
#'     island_size = 10,
#'     p = 0.5,
#'     edges_between = 1,
#'     set_seed = 23)
#'
#' # Get a count of nodes
#' islands_graph %>%
#'   count_nodes()
#'
#' # Get a count of edges
#' islands_graph %>%
#'   count_edges()
#' @importFrom igraph sample_islands
#' @importFrom dplyr select bind_cols as_tibble
#' @export add_islands_graph

add_islands_graph <- function(graph,
                              n_islands,
                              island_size,
                              p,
                              edges_between,
                              type = NULL,
                              label = TRUE,
                              rel = NULL,
                              node_aes = NULL,
                              edge_aes = NULL,
                              node_data = NULL,
                              edge_data = NULL,
                              set_seed = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  index__ <- id <- NULL

  # If a seed value is supplied, set a seed
  if (!is.null(set_seed)) {
    set.seed(set_seed, kind = "Mersenne-Twister")
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

  # Create the graph to be added
  sample_islands_graph <-
    igraph::sample_islands(
      islands.n = n_islands,
      islands.size = island_size,
      islands.pin = p,
      n.inter = edges_between)

  sample_islands_graph <- from_igraph(sample_islands_graph)

  # Add in a static `type` value for all new nodes
  if (!is.null(type)) {
    sample_islands_graph$nodes_df$type <- as.character(type[1])
  }

  # Add in a static `rel` value for all new nodes
  if (!is.null(rel)) {
    sample_islands_graph$edges_df$rel <- as.character(rel[1])
  }

  # If `label` is requested, use the node ID to
  # create a unique label for all new nodes
  if (label == TRUE) {
    sample_islands_graph$nodes_df$label <-
      sample_islands_graph$nodes_df$id %>% as.character()
  }

  n_nodes <- nrow(sample_islands_graph$nodes_df)

  n_edges <- nrow(sample_islands_graph$edges_df)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < nrow(sample_islands_graph$nodes_df)) {

      node_aes$index__ <- 1:nrow(sample_islands_graph$nodes_df)

      node_aes_tbl <-
        dplyr::as_tibble(node_aes) %>%
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

    node_data_tbl <- dplyr::as_tibble(node_data)

    if (nrow(node_data_tbl) < nrow(sample_islands_graph$nodes_df)) {

      node_data$index__ <- 1:nrow(sample_islands_graph$nodes_df)

      node_data_tbl <-
        dplyr::as_tibble(node_data) %>%
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

    edge_aes_tbl <- dplyr::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < nrow(sample_islands_graph$edges_df)) {

      edge_aes$index__ <- 1:nrow(sample_islands_graph$edges_df)

      edge_aes_tbl <-
        dplyr::as_tibble(edge_aes) %>%
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

    edge_data_tbl <- dplyr::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < nrow(sample_islands_graph$edges_df)) {

      edge_data$index__ <- 1:nrow(sample_islands_graph$edges_df)

      edge_data_tbl <-
        dplyr::as_tibble(edge_data) %>%
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

    sample_islands_graph$nodes_df <-
      sample_islands_graph$nodes_df %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    sample_islands_graph$nodes_df <-
      sample_islands_graph$nodes_df %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    sample_islands_graph$edges_df <-
      sample_islands_graph$edges_df %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    sample_islands_graph$edges_df <-
      sample_islands_graph$edges_df %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {
    graph <- combine_graphs(graph, sample_islands_graph)
  } else {
    graph <- sample_islands_graph
  }

  # Update the `last_node` counter
  graph$last_node <- nodes_created + n_nodes

  # Update the `last_edge` counter
  graph$last_edge <- edges_created + n_edges

  # Update the `graph_log` df with an action
  graph_log <-
    add_action_to_log(
      graph_log = graph_log,
      version_id = nrow(graph_log) + 1,
      function_used = "add_islands_graph",
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
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
