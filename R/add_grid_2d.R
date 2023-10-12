#' Add a 2D grid of nodes to the graph
#'
#' @description
#'
#' With a graph object of class `dgr_graph`, add a two-dimensional grid to the
#' graph.
#'
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param x The number of nodes in the x direction.
#' @param y The number of nodes in the y direction.
#' @param type An optional string that describes the entity type for the nodes
#'   to be added.
#' @param label Either a vector object of length `x * y` that provides optional
#'   labels for the new nodes, or, a logical value where setting to `TRUE`
#'   ascribes node IDs to the label and `FALSE` yields a blank label.
#' @param rel An optional string for providing a relationship label to all new
#'   edges created in the grid.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a new graph and add
#' # a 3 x 3 grid
#' graph <-
#'   create_graph() %>%
#'   add_grid_2d(
#'     x = 3, y = 3,
#'     type = "grid")
#'
#' # Get node information
#' # from this graph
#' graph %>%
#'   get_node_info()
#'
#' # Attributes can be specified
#' # in extra arguments and these
#' # are applied in order; Usually
#' # these attributes are applied
#' # to nodes (e.g., `type` is a
#' # node attribute) but the `rel`
#' # attribute will apply to the
#' # edges
#' graph_w_attrs <-
#'   create_graph() %>%
#'   add_grid_2d(
#'     x = 3, y = 2,
#'     label = c("one", "two",
#'               "three", "four",
#'               "five", "six"),
#'     type = c("a", "a",
#'              "b", "b",
#'              "c", "c"),
#'     rel = "grid",
#'     node_data = node_data(
#'       value = c(
#'         1.2, 8.4, 3.4,
#'         5.2, 6.1, 2.6)))
#'
#' # Get the graph's node data frame
#' graph_w_attrs %>% get_node_df()
#'
#' # Get the graph's edge data frame
#' graph_w_attrs %>% get_edge_df()
#'
#' @export
add_grid_2d <- function(
    graph,
    x,
    y,
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

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Stop if `x` is too small
  if (x < 2) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value for `x` must be at least 2.")
  }

  # Stop if `y` is too small
  if (y < 2) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value for `y` must be at least 2.")
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

  grid <-
    igraph::make_lattice(
      dimvector = c(x, y, 1),
      directed = graph_directed) %>%
    from_igraph()

  n_nodes <- grid %>% count_nodes()

  n_edges <- grid %>% count_edges()

  # Create a node data frame for the grid graph
  grid_nodes <-
    create_node_df(
      n = x * y,
      type = type,
      label = label)

  # Create an edge data frame for the grid graph
  grid_edges <-
    create_edge_df(
      from = grid %>%
        get_edge_df() %>%
        dplyr::pull(from),
      to = grid %>%
        get_edge_df() %>%
        dplyr::pull(to),
      rel = rel)

  # Create the grid graph
  grid_graph <-
    create_graph(
      directed = graph_directed,
      nodes_df = grid_nodes,
      edges_df = grid_edges)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < nrow(grid_graph$nodes_df)) {

      node_aes$index__ <- 1:nrow(grid_graph$nodes_df)

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

    if (nrow(node_data_tbl) < nrow(grid_graph$nodes_df)) {

      node_data$index__ <- 1:nrow(grid_graph$nodes_df)

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

    if (nrow(edge_aes_tbl) < nrow(grid_graph$edges_df)) {

      edge_aes$index__ <- 1:nrow(grid_graph$edges_df)

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

    if (nrow(edge_data_tbl) < nrow(grid_graph$edges_df)) {

      edge_data$index__ <- 1:nrow(grid_graph$edges_df)

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

    grid_graph$nodes_df <-
      grid_graph$nodes_df %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    grid_graph$nodes_df <-
      grid_graph$nodes_df %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    grid_graph$edges_df <-
      grid_graph$edges_df %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    grid_graph$edges_df <-
      grid_graph$edges_df %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {
    graph <- combine_graphs(graph, grid_graph)
  } else {
    graph <- grid_graph
  }

  # Update the `last_node` counter
  graph$last_node <- nodes_created + nrow(grid_nodes)

  # Update the `last_edge` counter
  graph$last_edge <- edges_created + nrow(grid_edges)

  # Update the `graph_log` df with an action
  graph_log <-
    add_action_to_log(
      graph_log = graph_log,
      version_id = nrow(graph_log) + 1,
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
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
