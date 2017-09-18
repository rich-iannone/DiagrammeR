#' Add a Watts-Strogatz small-world graph
#' @description To an existing graph object, add
#' a graph built according to the Watts-Strogatz
#' small-world model, which uses a lattice along
#' with a rewiring probability to randomly modify
#' edge definitions.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param dimension the dimension of the starting
#' lattice.
#' @param size the size of the lattice across each
#' dimension.
#' @param neighborhood the neighborhood where the
#' lattice nodes are to be connected.
#' @param p the rewiring probability.
#' @param loops a logical value (default is
#' \code{FALSE}) that governs whether loops are
#' allowed to be created.
#' @param multiple a logical value (default is
#' \code{FALSE}) that governs whether multiple
#' edges are allowed to be created.
#' @param set_seed supplying a value sets a random seed
#' of the \code{Mersenne-Twister} implementation.
#' @examples
#' # Create an undirected smallworld
#' # graph with 100 nodes using
#' # a probability value of 0.05
#' smallworld_graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_smallworld_graph(
#'     dimension = 1,
#'     size = 50,
#'     neighborhood = 1,
#'     p = 0.05,
#'     set_seed = 23)
#'
#' # Get a count of nodes
#' smallworld_graph %>%
#'   count_nodes()
#' #> [1] 50
#'
#' # Get a count of edges
#' smallworld_graph %>%
#'   count_edges()
#' #> [1] 50
#' @importFrom igraph sample_smallworld
#' @export add_smallworld_graph

add_smallworld_graph <- function(graph,
                                 dimension,
                                 size,
                                 neighborhood,
                                 p,
                                 loops = FALSE,
                                 multiple = FALSE,
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
  sample_smallworld_igraph <-
    igraph::sample_smallworld(
      dim = dimension,
      size = size,
      nei = neighborhood,
      p = p,
      loops = loops,
      multiple = multiple)

  sample_smallworld_graph <- from_igraph(sample_smallworld_igraph)

  n_nodes <- nrow(sample_smallworld_graph$nodes_df)

  n_edges <- nrow(sample_smallworld_graph$edges_df)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, sample_smallworld_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(n_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(n_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_smallworld_graph",
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
        function_used = "add_smallworld_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(sample_smallworld_graph$nodes_df),
        edges = nrow(sample_smallworld_graph$edges_df))

    sample_smallworld_graph$global_attrs <- global_attrs
    sample_smallworld_graph$graph_log <- graph_log
    sample_smallworld_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(sample_smallworld_graph$graph_actions) > 0) {
      sample_smallworld_graph <-
        sample_smallworld_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (sample_smallworld_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = sample_smallworld_graph)
    }

    return(sample_smallworld_graph)
  }
}
