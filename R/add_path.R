#' Add a path of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node path to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of nodes comprising the path.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node path.
#' @param node_aes an optional list of named vectors
#' comprising node aesthetic attributes. The helper
#' function \code{node_aes()} is strongly recommended
#' for use here as it contains arguments for each
#' of the accepted node aesthetic attributes (e.g.,
#' \code{shape}, \code{style}, \code{color},
#' \code{fillcolor}).
#' @param edge_aes an optional list of named vectors
#' comprising edge aesthetic attributes. The helper
#' function \code{edge_aes()} is strongly recommended
#' for use here as it contains arguments for each
#' of the accepted edge aesthetic attributes (e.g.,
#' \code{shape}, \code{style}, \code{penwidth},
#' \code{color}).
#' @param node_data an optional list of named vectors
#' comprising node data attributes. The helper
#' function \code{node_data()} is strongly recommended
#' for use here as it helps bind data specifically
#' to the created nodes.
#' @param edge_data an optional list of named vectors
#' comprising edge data attributes. The helper
#' function \code{edge_data()} is strongly recommended
#' for use here as it helps bind data specifically
#' to the created edges.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
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
#' node_info(graph)
#' #>   id type label deg indeg outdeg loops
#' #> 1  1 path     1   1     0      1     0
#' #> 2  2 path     2   2     1      1     0
#' #> 3  3 path     3   2     1      1     0
#' #> 4  4 path     4   1     1      0     0
#' #> 5  5 path     5   1     0      1     0
#' #> 6  6 path     6   2     1      1     0
#' #> 7  7 path     7   2     1      1     0
#' #> 8  8 path     8   2     1      1     0
#' #> 9  9 path     9   1     1      0     0
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
#'   add_path(
#'     n = 6,
#'     label = c("one", "two",
#'               "three", "four",
#'               "five", "six"),
#'     type = c("a", "a",
#'              "b", "b",
#'              "c", "c"),
#'     value = c(1.2, 8.4,
#'               3.4, 5.2,
#'               6.1, 2.6),
#'     rel = "path")
#'
#' # Get the graph's node data frame
#' get_node_df(graph_w_attrs)
#' #>   id type label value
#' #> 1  1    a   one   1.2
#' #> 2  2    a   two   8.4
#' #> 3  3    b three   3.4
#' #> 4  4    b  four   5.2
#' #> 5  5    c  five   6.1
#' #> 6  6    c   six   2.6
#'
#' # Get the graph's edge data frame
#' get_edge_df(graph_w_attrs)
#' #>   id from to  rel
#' #> 1  1    1  2 path
#' #> 2  2    2  3 path
#' #> 3  3    3  4 path
#' #> 4  4    4  5 path
#' #> 5  5    5  6 path
#' #> 6  6    6  1 path
#' }
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @export add_path

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
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop if n is too small
  if (n <= 1)  {
    stop("The value for `n` must be at least 2.")
  }

  # Create bindings for specific variables
  id <- index__ <- NULL

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
  nodes <- seq(1, n)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- tibble::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < n) {

      node_aes$index__ <- 1:n

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

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- tibble::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < (n - 1)) {

      edge_aes$index__ <- 1:(n - 1)

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

  # Collect node data attributes
  if (!is.null(node_data)) {

    node_data_tbl <- tibble::as_tibble(node_data)

    if (nrow(node_data_tbl) < n) {

      node_data$index__ <- 1:n

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

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- tibble::as_tibble(edge_data)

    if (nrow(edge_data_tbl) < (n - 1)) {

      edge_data$index__ <- 1:(n - 1)

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
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, path_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(path_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(path_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_path",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df),
        d_n = n,
        d_e = n - 1)

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
        function_used = "add_path",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(path_graph$nodes_df),
        edges = nrow(path_graph$edges_df),
        d_n = n,
        d_e = n - 1)

    path_graph$global_attrs <- global_attrs
    path_graph$graph_log <- graph_log
    path_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(path_graph$graph_actions) > 0) {
      path_graph <-
        path_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (path_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = path_graph)
    }

    return(path_graph)
  }
}
