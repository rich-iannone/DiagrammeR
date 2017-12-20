#' Add a preferential attachment graph
#' @description To an existing graph
#' object, add a graph built
#' according to the Barabasi-Albert
#' model, which uses preferential
#' attachment in its stochastic
#' algorithm.
#' @param graph a graph object of
#' class \code{dgr_graph}.
#' @param n the number of nodes
#' comprising the preferential
#' attachment graph.
#' @param m the number of edges to
#' add in each time step.
#' @param power the power of the
#' preferential attachment. The
#' default value of \code{1}
#' indicates a linear preferential
#' attachment.
#' @param out_dist a numeric vector
#' that provides the distribution of
#' the number of edges to add in each
#' time step.
#' @param use_total_degree a logical
#' value (default is \code{TRUE})
#' that governs whether the total
#' degree should be used for
#' calculating the citation
#' probability. If \code{FALSE}, the
#' indegree is used.
#' @param zero_appeal a measure of
#' the attractiveness of the nodes
#' with no adjacent edges.
#' @param algo the algorithm to use
#' to generate the graph. The
#' available options are
#' \code{psumtree},
#' \code{psumtree-multiple}, and
#' \code{bag}. With the \code{psumtree}
#' algorithm, a partial prefix-sum
#' tree is used to to create the
#' graph. Any values for \code{power}
#' and \code{zero_appeal} can be
#' provided and this algorithm never
#' generates multiple edges.
#' The \code{psumtree-multiple}
#' algorithm also uses a partial
#' prefix-sum tree but the difference
#' here is that multiple edges are
#' allowed. The \code{bag} algorithm
#' places the node IDs into a bag as
#' many times as their in-degree
#' (plus once more). The required
#' number of cited nodes are drawn
#' from the bag with replacement.
#' Multiple edges may be produced
#' using this method (it is not
#' disallowed).
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
#' @param set_seed supplying a value
#' sets a random seed of the
#' \code{Mersenne-Twister}
#' implementation.
#' @examples
#' # Create an undirected PA
#' # graph with 100 nodes, adding
#' # 2 edges at every time step
#' pa_graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_pa_graph(
#'     n = 100,
#'     m = 1)
#'
#' # Get a count of nodes
#' pa_graph %>%
#'   count_nodes()
#' #> [1] 100
#'
#' # Get a count of edges
#' pa_graph %>%
#'   count_edges()
#' #> [1] 99
#' @importFrom igraph sample_pa
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @export add_pa_graph

add_pa_graph <- function(graph,
                         n,
                         m = NULL,
                         power = 1,
                         out_dist = NULL,
                         use_total_degree = FALSE,
                         zero_appeal = 1,
                         algo = "psumtree",
                         node_aes = NULL,
                         edge_aes = NULL,
                         node_data = NULL,
                         edge_data = NULL,
                         set_seed = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  index__ <- id <- NULL

  # If a seed value is supplied, set a seed
  if (!is.null(set_seed)) {
    set.seed(set_seed, kind = "Mersenne-Twister")
  }

  # Stop if n is too small
  if (n <= 0)  {
    stop("The value for `n` must be at least 1.")
  }

  # Stop if the value for `algo` is not a
  # valid value
  if (!(algo %in% c("psumtree", "psumtree-multiple", "bag"))) {
    stop("The value given for `algo` must be either `psumtree`, `psumtree-multiple`, or `bag`.")
  }

  # If `bag` chosen as the algorithm, force
  # `power` and `zero_appeal` to both be 1
  if (algo == "bag") {
    power <- 1
    zero_appeal <- 1
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
  sample_pa_igraph <-
    igraph::sample_pa(
      n = n,
      power = power,
      m = m,
      out.dist = out_dist,
      out.pref = use_total_degree,
      zero.appeal = zero_appeal,
      directed = graph_directed,
      algorithm = algo)

  sample_pa_graph <- from_igraph(sample_pa_igraph)

  # Add in a static `type` value for all new nodes
  if (!is.null(type)) {
    sample_pa_graph$nodes_df$type <- as.character(type[1])
  }

  # Add in a static `rel` value for all new nodes
  if (!is.null(rel)) {
    sample_pa_graph$edges_df$rel <- as.character(rel[1])
  }

  # If `label` is requested, use the node ID to
  # create a unique label for all new nodes
  if (label == TRUE) {
    sample_pa_graph$nodes_df$label <-
      sample_pa_graph$nodes_df$id %>% as.character()
  }

  n_nodes <- nrow(sample_pa_graph$nodes_df)

  n_edges <- nrow(sample_pa_graph$edges_df)

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- tibble::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < nrow(sample_pa_graph$nodes_df)) {

      node_aes$index__ <- 1:nrow(sample_pa_graph$nodes_df)

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

    if (nrow(node_data_tbl) < nrow(sample_pa_graph$nodes_df)) {

      node_data$index__ <- 1:nrow(sample_pa_graph$nodes_df)

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

    if (nrow(edge_aes_tbl) < nrow(sample_pa_graph$edges_df)) {

      edge_aes$index__ <- 1:nrow(sample_pa_graph$edges_df)

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

    if (nrow(edge_data_tbl) < nrow(sample_pa_graph$edges_df)) {

      edge_data$index__ <- 1:nrow(sample_pa_graph$edges_df)

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

    sample_pa_graph$nodes_df <-
      sample_pa_graph$nodes_df %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    sample_pa_graph$nodes_df <-
      sample_pa_graph$nodes_df %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    sample_pa_graph$edges_df <-
      sample_pa_graph$edges_df %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    sample_pa_graph$edges_df <-
      sample_pa_graph$edges_df %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, sample_pa_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(n_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(n_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_pa_graph",
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
        function_used = "add_pa_graph",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(sample_pa_graph$nodes_df),
        edges = nrow(sample_pa_graph$edges_df),
        d_n = n_nodes,
        d_e = n_edges)

    sample_pa_graph$global_attrs <- global_attrs
    sample_pa_graph$graph_log <- graph_log
    sample_pa_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(sample_pa_graph$graph_actions) > 0) {
      sample_pa_graph <-
        sample_pa_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (sample_pa_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = sample_pa_graph)
    }

    return(sample_pa_graph)
  }
}
