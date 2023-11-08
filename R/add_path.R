#' Add a path of nodes to the graph
#'
#' With a graph object of class `dgr_graph`, add a node path to the graph.
#'
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param n The number of nodes comprising the path.
#' @param type An optional string that describes the entity type for the nodes
#'   to be added.
#' @param label Either a vector object of length `n` that provides optional
#'   labels for the new nodes, or, a logical value where setting to `TRUE`
#'   ascribes node IDs to the label and `FALSE` yields a blank label.
#' @param rel An optional string for providing a relationship label to all new
#'   edges created in the node path.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a new graph and add
#' # 2 paths of varying lengths
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 4,
#'     type = "path") %>%
#'   add_path(
#'     n = 5,
#'     type = "path")
#'
#' # Get node information
#' # from this graph
#' graph %>% get_node_info()
#'
#' # Node and edge aesthetic and data
#' # attributes can be specified in
#' # the `node_aes`, `edge_aes`,
#' # `node_data`, and `edge_data`
#' # arguments
#'
#' suppressWarnings(RNGversion("3.5.0"))
#' set.seed(23)
#'
#' graph_w_attrs <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     label = c(
#'       "one", "two", "three"),
#'     type = c(
#'       "a", "a", "b"),
#'     rel = "A",
#'     node_aes = node_aes(
#'       fillcolor = "steelblue"),
#'     edge_aes = edge_aes(
#'       color = "red",
#'       penwidth = 1.2),
#'     node_data = node_data(
#'       value = c(
#'         1.6, 2.8, 3.4)),
#'     edge_data = edge_data(
#'       value =
#'         rnorm(
#'           n = 2,
#'           mean = 5.0,
#'           sd = 1.0)))
#'
#' # Get the graph's node data frame
#' graph_w_attrs %>% get_node_df()
#'
#' # Get the graph's edge data frame
#' graph_w_attrs %>% get_edge_df()
#'
#' @export
add_path <- function(graph,
                     n,
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
  check_graph_valid(graph)

  # Stop if n is too small
  check_number_whole(n, min = 2)

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
  nodes <- seq_len(n)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < n) {

      node_aes$index__ <- seq_len(n)

      node_aes_tbl <-
        dplyr::as_tibble(node_aes) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(node_aes_tbl)) {
      node_aes_tbl$id <- NULL
    }
  }

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- dplyr::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < (n - 1)) {

      edge_aes$index__ <-seq_len(n - 1)

      edge_aes_tbl <-
        dplyr::as_tibble(edge_aes) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(edge_aes_tbl)) {
      edge_aes_tbl$id <- NULL
    }
  }

  # Collect node data attributes
  if (!is.null(node_data)) {

    node_data_tbl <- dplyr::as_tibble(node_data)

    if (nrow(node_data_tbl) < n) {

      node_data$index__ <- seq_len(n)

      node_data_tbl <-
        dplyr::as_tibble(node_data) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(node_data_tbl)) {
      node_data_tbl$id <- NULL
    }
  }

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- dplyr::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < (n - 1)) {

      edge_data$index__ <- seq_len(n - 1)

      edge_data_tbl <-
        dplyr::as_tibble(edge_data) %>%
        dplyr::select(-"index__")
    }

    if ("id" %in% colnames(edge_data_tbl)) {
      edge_data_tbl$id <- NULL
    }
  }

  # Create a node data frame for the path graph
  path_nodes <-
    create_node_df(
      n = n,
      type = type,
      label = label)

  # Add node aesthetics if available
  if (exists("node_aes_tbl")) {

    path_nodes <-
      path_nodes %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    path_nodes <-
      path_nodes %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Create an edge data frame for the path graph
  path_edges <-
    create_edge_df(
      from = nodes[1:length(nodes) - 1],
      to = nodes[2:length(nodes)],
      rel = rel)

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    path_edges <-
      path_edges %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    path_edges <-
      path_edges %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # Create the path graph
  path_graph <-
    create_graph(
      directed = graph_directed,
      nodes_df = path_nodes,
      edges_df = path_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (is_graph_empty(graph)) {
    graph <- path_graph
  } else {
    graph <- combine_graphs(graph, path_graph)
  }

  # Update the `last_node` counter
  graph$last_node <- nodes_created + nrow(path_nodes)

  # Update the `last_edge` counter
  graph$last_edge <- edges_created + nrow(path_edges)

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
      d_n = n,
      d_e = n - 1)

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
