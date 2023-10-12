#' Use the breadth-first search (bfs) algorithm
#'
#' @description
#'
#' With a chosen or random node serving as the starting point, perform a
#' breadth-first search of the whole graph and return the node ID values
#' visited. The bfs algorithm differs from depth-first search (dfs) in that bfs
#' will follow tree branches branches one level at a time until terminating at
#' leaf node (dfs traverses branches as far as possible).
#'
#' @inheritParams render_graph
#' @param node An optional node ID value to specify a single starting point for
#'   the bfs. If not provided, a random node from the graph will be chosen.
#' @param direction Using `all` (the default), the bfs will ignore edge
#'   direction while traversing through the graph. With `out` and `in`,
#'   traversals between adjacent nodes will respect the edge direction.
#'
#' @return A vector containing node ID values for nodes visited during the
#'   breadth-first search. The order of the node IDs corresponds to the order
#'   visited.
#'
#' @examples
#' # Create a graph containing
#' # two balanced trees
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 2) %>%
#'   add_balanced_tree(
#'     k = 3, h = 2)
#'
#' # Perform a breadth-first
#' # search of the graph,
#' # beginning at the root node
#' # `1` (the default
#' # `direction = "all"` doesn't
#' # take edge direction into
#' # account)
#' graph %>%
#'   do_bfs(node = 1)
#'
#' # If not specifying a
#' # starting node, the function
#' # will begin the search from
#' # a random node
#' graph %>%
#'   do_bfs()
#'
#' # It's also possible to
#' # perform bfs while taking
#' # into account edge direction;
#' # using `direction = "in"`
#' # causes the bfs routine to
#' # visit nodes along inward edges
#' graph %>%
#'   do_bfs(
#'     node = 1,
#'     direction = "in")
#'
#' # Using `direction = "out"`
#' # results in the bfs moving
#' # along solely outward edges
#' graph %>%
#'   do_bfs(
#'     node = 1,
#'     direction = "out")
#'
#' @export
do_bfs <- function(
    graph,
    node = NULL,
    direction = "all"
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Validation: Graph contains nodes
  if (!graph_contains_nodes(graph)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes.")
  }

  # If no node provided, choose a random node
  if (is.null(node)) {
    node <- sample(graph$nodes_df[, 1], 1)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Perform the breadth-first search algorithm in
  # the direction requested
  if (direction == "all") {

    bfs_result <-
      igraph::bfs(
        graph = ig_graph,
        root = node,
        mode = "all")

  } else if (direction == "out") {

    bfs_result <-
      igraph::bfs(
        graph = ig_graph,
        root = node,
        mode = "out")

  } else if (direction == "in") {

    bfs_result <-
      igraph::bfs(
        graph = ig_graph,
        root = node,
        mode = "in")

  } else if (!(direction %in% c("all", "out", "in"))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value for `direction` must be either `all`, `out`, or `in`")
  }

  # Get the nodes visited during the bfs
  as.integer(as.numeric(bfs_result$order))
}
