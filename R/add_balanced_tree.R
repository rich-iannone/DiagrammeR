#' Add a balanced tree of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a balanced tree to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param k the branching factor for the tree.
#' @param h the height of the tree.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node tree.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add 2 different types of
#' # balanced trees of height 2 (branching twice) and
#' # different branching ratios
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2, "binary") %>%
#'   add_balanced_tree(3, 2, "tertiary")
#'
#' # Get some node information from this graph
#' node_info(graph) %>% head(5)
#' #>    id     type label deg indeg outdeg loops
#' #> 1   1   binary     1   2     0      2     0
#' #> 2   2   binary     2   3     1      2     0
#' #> 3   3   binary     3   3     1      2     0
#' #> 4   4   binary     4   1     1      0     0
#' #> 5   5   binary     5   1     1      0     0
#' @export add_balanced_tree

add_balanced_tree <- function(graph,
                              k,
                              h,
                              type = NULL,
                              label = TRUE,
                              rel = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop if k is too small
  if (k <= 1) {
    stop("The value for k must be at least 2.")
  }

  # Stop if h is too small
  if (h <= 1) {
    stop("The value for h must be at least 2.")
  }

  # Determine the number of nodes in the balanced tree
  n_nodes_tree <-
    (k^(h + 1) - 1) / (k - 1)

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

  # Create the tree graph
  tree_graph <- create_graph(tree_nodes, tree_edges)

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
        edges = nrow(combined_graph$edges_df))

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
        edges = nrow(tree_graph$edges_df))

    tree_graph$global_attrs <- global_attrs
    tree_graph$graph_log <- graph_log
    tree_graph$graph_info <- graph_info

    # Write graph backup if the option is set
    if (tree_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = tree_graph)
    }

    return(tree_graph)
  }
}
