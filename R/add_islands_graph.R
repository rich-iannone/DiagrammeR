#' Create a random growing graph with m edges between islands
#' @description To an existing graph object, add
#' several Erdos-Renyi random graphs (the islands)
#' using a common set of parameters, connected
#' together by a fixed number of edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n_islands the number of islands
#' in the generated graph.
#' @param island_size the size of the islands
#' in the generated graph.
#' @param p the probability of there being
#' edges between the islands.
#' @param edges_between The number of edges
#' between islands.
#' @param set_seed supplying a value sets a random seed
#' of the \code{Mersenne-Twister} implementation.
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
#' #> [1] 40
#'
#' # Get a count of edges
#' islands_graph %>%
#'   count_edges()
#' #> [1] 98
#' @importFrom igraph sample_islands
#' @export add_islands_graph

add_islands_graph <- function(graph,
                              n_islands,
                              island_size,
                              p,
                              edges_between,
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
  sample_islands_igraph <-
    igraph::sample_islands(
      islands.n = n_islands,
      islands.size = island_size,
      islands.pin = p,
      n.inter = edges_between)

  sample_islands_igraph <- from_igraph(sample_islands_igraph)

  n_nodes <- nrow(sample_islands_igraph$nodes_df)

  n_edges <- nrow(sample_islands_igraph$edges_df)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, sample_islands_igraph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(n_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(n_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_islands_graph",
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
        function_used = "add_islands_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(sample_islands_igraph$nodes_df),
        edges = nrow(sample_islands_igraph$edges_df))

    sample_islands_igraph$global_attrs <- global_attrs
    sample_islands_igraph$graph_log <- graph_log
    sample_islands_igraph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(sample_islands_igraph$graph_actions) > 0) {
      sample_islands_igraph <-
        sample_islands_igraph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (sample_islands_igraph$graph_info$write_backups) {
      save_graph_as_rds(graph = sample_islands_igraph)
    }

    return(sample_islands_igraph)
  }
}
