#' Perform the breadth-first search (bfs) algorithm
#' @description With a chosen or random node
#' serving as the starting point, perform a
#' breadth-first search of the whole graph and return
#' the node ID values visited. The bfs algorithm
#' differs from depth-first search (dfs) in that
#' bfs will follow tree branches branches one level
#' at a time until terminating at leaf node (dfs
#' traverses branches as far as possible).
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node an optional node ID value to specify
#' a single starting point for the bfs. If not
#' provided, a random node from the graph will be
#' chosen.
#' @param direction using \code{all} (the default),
#' the bfs will ignore edge direction while
#' traversing through the graph. With \code{out},
#' traversals between adjacent nodes will respect
#' the edge direction.
#' @return a vector containing node ID values for
#' nodes visited during the breadth-first search. The
#' order of the node IDs corresponds to the order
#' visited.
#' @examples
#' # Create a graph containing two balanced trees
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2) %>%
#'   add_balanced_tree(3, 2)
#'
#' # Perform a breadth-first search of the graph,
#' # beginning at the root node `1` (the default
#' # `direction = "all"` doesn't take edge
#' # direction into account)
#' graph %>% do_bfs(1)
#' #> [1]  1  2  3  4  5  6  7  8  9 10 11 12 13
#' #> [14] 14 15 16 17 18 19 20
#'
#' # If not specifying a starting node, the function
#' # will begin the search from a random node
#' graph %>% do_bfs()
#' #> [1]  6  3  1  7  2  4  5  8  9 10 11 12 13
#' #> [14] 14 15 16 17 18 19 20
#'
#' # It's also possible to perform bfs while
#' # taking into account edge direction; using
#' # `direction = "in"` causes the bfs routine to
#' # visit nodes along inward edges
#' graph %>% do_bfs(1, "in")
#' #> [1]  1  2  3  4  5  6  7  8  9 10 11 12 13
#' #> [14] 14 15 16 17 18 19 20
#'
#' # Using `direction = "out"` results in the bfs
#' # moving along solely outward edges
#' graph %>% do_bfs(1, "out")
#' #> [1]  1  2  3  4  5  6  7  8  9 10 11 12 13
#' #> [14] 14 15 16 17 18 19 20
#' @importFrom igraph bfs
#' @export do_bfs

do_bfs <- function(graph,
                   node = NULL,
                   direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes.")
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
        neimode = "all")
  } else if (direction == "out") {
    bfs_result <-
      igraph::bfs(
        graph = ig_graph,
        root = node,
        neimode = "out")
  } else if (direction == "in") {
    bfs_result <-
      igraph::bfs(
        graph = ig_graph,
        root = node,
        neimode = "in")
  } else if (!(direction %in% c("all", "out", "in"))) {
    stop("The value for `direction` must be either `all`, `out`, or `in`.")
  }

  # Get the nodes visited during the bfs
  bfs_nodes <- as.integer(as.numeric(bfs_result$order))

  return(bfs_nodes)
}
