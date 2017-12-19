#' Add a balanced tree to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a balanced tree
#' to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param k the branching factor for
#' the tree.
#' @param h the height of the tree.
#' @param type an optional string that
#' describes the entity type for the
#' nodes to be added.
#' @param label either a vector object
#' of length \code{n} that provides
#' optional labels for the new nodes, or,
#' a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the
#' label and \code{FALSE} yields a blank
#' label.
#' @param rel an optional string for
#' providing a relationship label to all
#' new edges created in the node tree.
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
#' @return a graph object of class
#' \code{dgr_graph}.
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
#' node_info(graph) %>%
#'   head(5)
#' #>   id   type label deg indeg outdeg loops
#' #> 1  1 binary     1   2     0      2     0
#' #> 2  2 binary     2   3     1      2     0
#' #> 3  3 binary     3   3     1      2     0
#' #> 4  4 binary     4   1     1      0     0
#' #> 5  5 binary     5   1     1      0     0
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
#' get_node_df(graph_w_attrs) %>%
#'   head(3)
#' #>   id type label fillcolor value
#' #> 1  1    a   one steelblue   1.6
#' #> 2  2    b   two steelblue   2.8
#' #> 3  3    b three steelblue   3.4
#'
#' # Get the first three rows of
#' # the graph's edge data frame
#' get_edge_df(graph_w_attrs) %>%
#'   head(3)
#' #>   id from to rel penwidth color
#' #> 1  1    1  2   A      1.2   red
#' #> 2  2    1  3   A      1.2   red
#' #> 3  3    2  4   A      1.2   red
#' @export add_balanced_tree

add_balanced_tree <- function(graph,
                              k,
                              h,
                              type = NULL,
                              label = TRUE,
                              rel = NULL,
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

  # Stop if k is too small
  if (k <= 1) {
    stop("The value for `k` must be at least 2.")
  }

  # Stop if h is too small
  if (h <= 1) {
    stop("The value for `h` must be at least 2.")
  }

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

    node_aes_tbl <- tibble::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < n_nodes_tree) {

      node_aes$index__ <- 1:n_nodes_tree

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

    if (nrow(node_data_tbl) < n_nodes_tree) {

      node_data$index__ <- 1:n_nodes_tree

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

    if (nrow(edge_aes_tbl) < n_edges_tree) {

      edge_aes$index__ <- 1:n_edges_tree

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

    if (nrow(edge_data_tbl) < n_edges_tree) {

      edge_data$index__ <- 1:n_edges_tree

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

    # Combine the graphs
    combined_graph <- combine_graphs(graph, tree_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(tree_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(tree_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_balanced_tree",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df),
        d_n = n_nodes_tree,
        d_e = n_edges_tree)

    combined_graph$global_attrs <- global_attrs
    combined_graph$graph_log <- graph_log
    combined_graph$graph_info <- graph_info

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
        function_used = "add_balanced_tree",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(tree_graph$nodes_df),
        edges = nrow(tree_graph$edges_df),
        d_n = n_nodes_tree,
        d_e = n_edges_tree)

    tree_graph$global_attrs <- global_attrs
    tree_graph$graph_log <- graph_log
    tree_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(graph$graph_actions) > 0) {
      graph <-
        graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (tree_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = tree_graph)
    }

    tree_graph
  }
}
