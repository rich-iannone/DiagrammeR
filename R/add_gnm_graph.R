#' Add a G(n, m) Erdos-Renyi graph
#' @description To an existing graph object, add
#' a graph built according to the Erdos-Renyi
#' G(n, m) model. This uses the same constant
#' probability when creating the fixed number
#' of edges. Thus for \code{n} nodes there will
#' be \code{m} edges and, if loops is set as
#' \code{TRUE}, then random loop edges will be
#' part of \code{m}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of nodes comprising the
#' generated graph.
#' @param m the number of edges in the
#' generated graph.
#' @param loops a logical value (default is
#' \code{FALSE}) that governs whether loops are
#' allowed to be created.
#' @param set_seed supplying a value sets a random seed
#' of the \code{Mersenne-Twister} implementation.
#' @examples
#' # Create an undirected GNM
#' # graph with 100 nodes and
#' # 120 edges
#' gnm_graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 100,
#'     m = 120)
#'
#' # Get a count of nodes
#' gnm_graph %>%
#'   count_nodes()
#' #> [1] 100
#'
#' # Get a count of edges
#' gnm_graph %>%
#'   count_edges()
#' #> [1] 120
#' @importFrom igraph sample_gnm
#' @export add_gnm_graph

add_gnm_graph <- function(graph,
                          n,
                          m,
                          loops = FALSE,
                          set_seed = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

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
  sample_gnm_igraph <-
    igraph::sample_gnm(
      n = n,
      m = m,
      directed = graph_directed,
      loops = loops)

  sample_gnm_graph <- from_igraph(sample_gnm_igraph)

  n_nodes <- nrow(sample_gnm_graph$nodes_df)

  n_edges <- nrow(sample_gnm_graph$edges_df)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, sample_gnm_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(n_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(n_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_gnm_graph",
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
        function_used = "add_gnm_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(sample_gnm_graph$nodes_df),
        edges = nrow(sample_gnm_graph$edges_df),
        d_n = n_nodes,
        d_e = n_edges)

    sample_gnm_graph$global_attrs <- global_attrs
    sample_gnm_graph$graph_log <- graph_log
    sample_gnm_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(sample_gnm_graph$graph_actions) > 0) {
      sample_gnm_graph <-
        sample_gnm_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (sample_gnm_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = sample_gnm_graph)
    }

    return(sample_gnm_graph)
  }
}
