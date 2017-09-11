#' Add a G(n, p) Erdos-Renyi graph
#' @description To an existing graph object, add
#' a graph built according to the Erdos-Renyi
#' G(n, p) model, which uses a constant probability
#' when creating edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of nodes comprising the
#' generated graph.
#' @param p the probability of creating an edge
#' between two arbitrary nodes.
#' @param loops a logical value (default is
#' \code{FALSE}) that governs whether loops are
#' allowed to be created.
#' @param set_seed supplying a value sets a random seed
#' of the \code{Mersenne-Twister} implementation.
#' @examples
#' # Create an undirected GNP
#' # graph with 100 nodes using
#' # a probability value of 0.05
#' gnp_graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnp_graph(
#'     n = 100,
#'     p = 0.05)
#'
#' # Get a count of nodes
#' gnp_graph %>% node_count()
#' #> [1] 100
#'
#' # Get a count of edges
#' gnp_graph %>% edge_count()
#' #> [1] 216
#' @importFrom igraph sample_gnp
#' @export add_gnp_graph

add_gnp_graph <- function(graph,
                          n,
                          p,
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
  sample_gnp_igraph <-
    igraph::sample_gnp(
      n = n,
      p = p,
      directed = graph_directed,
      loops = loops)

  sample_gnp_graph <- from_igraph(sample_gnp_igraph)

  n_nodes <- nrow(sample_gnp_graph$nodes_df)

  n_edges <- nrow(sample_gnp_graph$edges_df)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, sample_gnp_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(n_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(n_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_gnp_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df))

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
        function_used = "add_gnp_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(sample_gnp_graph$nodes_df),
        edges = nrow(sample_gnp_graph$edges_df))

    sample_gnp_graph$global_attrs <- global_attrs
    sample_gnp_graph$graph_log <- graph_log
    sample_gnp_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(sample_gnp_graph$graph_actions) > 0) {
      sample_gnp_graph <-
        sample_gnp_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (sample_gnp_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = sample_gnp_graph)
    }

    return(sample_gnp_graph)
  }
}
