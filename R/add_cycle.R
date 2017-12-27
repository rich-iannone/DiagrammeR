#' Add a cycle of nodes to the graph
#' @description With a graph object
#' of class \code{dgr_graph}, add a
#' node cycle to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of nodes
#' comprising the cycle.
#' @param type an optional string
#' that describes the entity type
#' for the nodes to be added.
#' @param label either a vector object
#' of length \code{n} that provides
#' optional labels for the new nodes,
#' or, a boolean value where setting
#' to \code{TRUE} ascribes node IDs
#' to the label and \code{FALSE}
#' yields a blank label.
#' @param rel an optional string for
#' providing a relationship label to
#' all new edges created in the node
#' cycle.
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
#' # add a cycle of nodes to it
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 6)
#'
#' # Get node information
#' # from this graph
#' node_info(graph)
#' #>   id type label deg indeg outdeg loops
#' #> 1  1 <NA>     1   2     1      1     0
#' #> 2  2 <NA>     2   2     1      1     0
#' #> 3  3 <NA>     3   2     1      1     0
#' #> 4  4 <NA>     4   2     1      1     0
#' #> 5  5 <NA>     5   2     1      1     0
#' #> 6  6 <NA>     6   2     1      1     0
#'
#' # Node and edge aesthetic and data
#' # attributes can be specified in
#' # the `node_aes`, `edge_aes`,
#' # `node_data`, and `edge_data`
#' # arguments
#'
#' set.seed(23)
#'
#' graph_w_attrs <-
#'   create_graph() %>%
#'   add_cycle(
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
#'           n = 3,
#'           mean = 5.0,
#'           sd = 1.0)))
#'
#' # Get the graph's node data frame
#' graph_w_attrs %>%
#'   get_node_df()
#' #>   id type label fillcolor value
#' #> 1  1    a   one steelblue   1.6
#' #> 2  2    a   two steelblue   2.8
#' #> 3  3    b three steelblue   3.4
#'
#' # Get the graph's edge data frame
#' graph_w_attrs %>%
#'   get_edge_df()
#' #>   id from to rel penwidth color    value
#' #> 1  1    1  2   A      1.2   red 5.996605
#' #> 2  2    2  3   A      1.2   red 6.107490
#' #> 3  3    3  1   A      1.2   red 4.721914
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @export add_cycle

add_cycle <- function(graph,
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
  if (n <= 2)  {
    stop("The value for `n` must be at least 3.")
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

    if (nrow(edge_aes_tbl) < n) {

      edge_aes$index__ <- 1:n

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

    if (nrow(edge_data_tbl) < n) {

      edge_data$index__ <- 1:n

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

  # Create a node data frame for the cycle graph
  cycle_nodes <-
    create_node_df(
      n = n,
      type = type,
      label = label)

  # Add node aesthetics if available
  if (exists("node_aes_tbl")) {

    cycle_nodes <-
      cycle_nodes %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    cycle_nodes <-
      cycle_nodes %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Create an edge data frame for the cycle graph
  cycle_edges <-
    create_edge_df(
      from = nodes,
      to = c(nodes[2:length(nodes)], nodes[1]),
      rel = rel)

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    cycle_edges <-
      cycle_edges %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    cycle_edges <-
      cycle_edges %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # Create the cycle graph
  cycle_graph <-
    create_graph(
      directed = graph_directed,
      nodes_df = cycle_nodes,
      edges_df = cycle_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, cycle_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(cycle_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(cycle_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_cycle",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df),
        d_n = n,
        d_e = n)

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
        function_used = "add_cycle",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(cycle_graph$nodes_df),
        edges = nrow(cycle_graph$edges_df),
        d_n = n,
        d_e = n)

    cycle_graph$global_attrs <- global_attrs
    cycle_graph$graph_log <- graph_log
    cycle_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(cycle_graph$graph_actions) > 0) {
      cycle_graph <-
        cycle_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (cycle_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = cycle_graph)
    }

    return(cycle_graph)
  }
}
