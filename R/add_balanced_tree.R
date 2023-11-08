#' Add a balanced tree to the graph
#'
#' @description
#'
#' With a graph object of class `dgr_graph`, add a balanced tree to the graph.
#'
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param k The branching factor for the tree.
#' @param h The height of the tree.
#' @param type An optional string that describes the entity type for the nodes
#'   to be added.
#' @param label Either a vector object of length `n` that provides optional
#'   labels for the new nodes, or, a boolean value where setting to `TRUE`
#'   ascribes node IDs to the label and `FALSE` yields a blank label.
#' @param rel An optional string for providing a relationship label to all new
#'   edges created in the node tree.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a new graph and
#' # add 2 different types of
#' # balanced trees of height
#' # 2 (branching twice) and
#' # different branching ratios
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2,
#'     h = 2,
#'     type = "binary") %>%
#'   add_balanced_tree(
#'     k = 3,
#'     h = 2,
#'     type = "tertiary")
#'
#' # Get some node information
#' # from this graph
#' graph %>%
#'   get_node_info() %>%
#'   head(5)
#'
#' # Node and edge aesthetic and data
#' # attributes can be specified in
#' # the `node_aes`, `edge_aes`,
#' # `node_data`, and `edge_data`
#' # arguments
#' graph_w_attrs <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2,
#'     h = 2,
#'     label = c(
#'       "one", "two",
#'       "three", "four",
#'       "five", "six", "seven"),
#'     type = c(
#'       "a", "b", "b", "c",
#'       "c", "c", "c"),
#'     rel = "A",
#'     node_aes = node_aes(
#'       fillcolor = "steelblue"),
#'     node_data = node_data(
#'       value = c(
#'         1.6, 2.8, 3.4, 8.3,
#'         3.8, 5.2, 3.2)),
#'     edge_aes = edge_aes(
#'       color = "red",
#'       penwidth = 1.2))
#'
#' # Get the first three rows of
#' # the graph's node data frame
#' graph_w_attrs %>%
#'   get_node_df() %>%
#'   head(3)
#'
#' # Get the first three rows of
#' # the graph's edge data frame
#' graph_w_attrs %>%
#'   get_edge_df() %>%
#'   head(3)
#'
#' @export
add_balanced_tree <- function(
    graph,
    k,
    h,
    type = NULL,
    label = TRUE,
    rel = NULL,
    node_aes = NULL,
    edge_aes = NULL,
    node_data = NULL,
    edge_data = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Stop if k or n is too small
  check_number_whole(k, min = 2)
  check_number_whole(h, min = 2)

  # Determine the number of nodes in the balanced tree
  n_nodes_tree <-
    (k^(h + 1) - 1) / (k - 1)

  # Determine the number of edges in the balanced tree
  n_edges_tree <- n_nodes_tree - 1

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

  # Get the sequence of nodes required
  nodes <- seq(1, n_nodes_tree)

  # Create a node data frame for the tree graph
  tree_nodes <-
    create_node_df(
      n = n_nodes_tree,
      type = type,
      label = label)

  # Create an edge data frame for the tree graph
  tree_edges <-
    create_edge_df(
      from = sort(
        rep(seq(nodes[1],
                nodes[length(
                  seq(nodes[2],
                      nodes[length(nodes)]))/k]), k)),
      to = seq(nodes[2], nodes[length(nodes)]),
      rel = rel)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < n_nodes_tree) {

      node_aes$index__ <- seq_len(n_nodes_tree)

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

    if (nrow(node_data_tbl) < n_nodes_tree) {

      node_data$index__ <- seq_len(n_nodes_tree)

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

    if (nrow(edge_aes_tbl) < n_edges_tree) {

      edge_aes$index__ <- seq_len(n_edges_tree)

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

    if (nrow(edge_data_tbl) < n_edges_tree) {

      edge_data$index__ <- seq_len(n_edges_tree)

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

    tree_nodes <-
      tree_nodes %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    tree_nodes <-
      tree_nodes %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    tree_edges <-
      tree_edges %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    tree_edges <-
      tree_edges %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # Create the tree graph
  tree_graph <-
    create_graph(
      directed = graph_directed,
      nodes_df = tree_nodes,
      edges_df = tree_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {
    graph <- combine_graphs(graph, tree_graph)
  } else {
    graph <- tree_graph
  }

  # Update the `last_node` counter
  graph$last_node <- nodes_created + nrow(tree_nodes)

  # Update the `last_edge` counter
  graph$last_edge <- edges_created + nrow(tree_edges)

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
      d_n = n_nodes_tree,
      d_e = n_edges_tree)

  graph$global_attrs <- global_attrs
  graph$graph_log <- graph_log
  graph$graph_info <- graph_info

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
