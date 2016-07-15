#' Is the graph a connected graph?
#' @description Determines whether a graph is a
#' connected graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # This graph, created using `create_random_graph()`
#' # is almost fully connected but there is an
#' # isolated node with no edges
#' graph_1 <-
#'   create_random_graph(
#'     30, 50, set_seed = 1)
#'
#' graph_1 %>% is_graph_connected
#' #> [1] FALSE
#'
#' # The following graph is fully connected
#' graph_2 <-
#'   create_random_graph(
#'     36, 50, set_seed = 1)
#'
#' graph_2 %>% is_graph_connected
#' #> [1] TRUE
#'
#' # Modify `graph_2` so that there are two
#' # clusters of nodes (i.e., making the graph
#' # not connected)
#' graph_3 <-
#'   graph_2 %>%
#'   delete_edge(10, 36) %>%
#'   delete_edge(25, 27) %>%
#'   delete_edge(28, 29) %>%
#'   delete_edge(4, 29) %>%
#'   delete_edge(24, 32)
#'
#' graph_3 %>% is_graph_connected
#' #> [1] FALSE
#' }
#' @importFrom utils combn
#' @export is_graph_connected

is_graph_connected <- function(graph) {

  # if any nodes have degree zero, then
  # the graph is not connected
  if (any(node_info(graph)[, 4] == 0)){
    return(FALSE)
  }

  node_pairs <-
    as.data.frame(t(utils::combn(get_nodes(graph), 2)),
                  stringsAsFactors = FALSE)
  graph_edges <-
    get_edges(graph, return_type = "df")

  colnames(node_pairs) <- colnames(graph_edges) <-
    c("x", "y")

  # Remove pairs of paths from `node_pairs` if there
  # are already edges between those pairs of nodes
  for (i in 1:nrow(graph_edges)) {

    edge_1 <-
      intersect(
        which(node_pairs[, 1] %in% graph_edges[i, 1]),
        which(node_pairs[, 2] %in% graph_edges[i, 2]))

    edge_2 <-
      intersect(
        which(node_pairs[, 2] %in% graph_edges[i, 1]),
        which(node_pairs[, 1] %in% graph_edges[i, 2]))

    path_to_remove <- c(edge_1, edge_2)

    if (length(path_to_remove == 1)){
      node_pairs <- node_pairs[-path_to_remove, ]
    }
  }

  traversal_distance <-
    as.integer((node_count(graph)/edge_count(graph)) *
                 max(node_info(graph)[, 4]))

  x <- 1
  repeat {

    # Randomly choose a node from the graph and
    # traverse in both directions to other nodes
    graph <-
      select_nodes_by_id(
        graph,
        sample(unique(c(node_pairs[, 1],
                        node_pairs[, 2])), 1))

    random_selection <- get_selection(graph)

    nodes_in_path <- vector(mode = "character")

    for (i in 1:traversal_distance) {
      graph <- trav_both(graph)
      nodes_in_path <-
        c(nodes_in_path, get_selection(graph))
    }

    graph <- clear_selection(graph)

    nodes_in_path <- unique(nodes_in_path)

    paths_to_remove <-
      data.frame(x = random_selection,
                 y = nodes_in_path,
                 stringsAsFactors = FALSE)

    for (i in 1:nrow(paths_to_remove)) {

      edge_1 <-
        intersect(
          which(node_pairs[, 1] %in% paths_to_remove[i, 1]),
          which(node_pairs[, 2] %in% paths_to_remove[i, 2]))

      edge_2 <-
        intersect(
          which(node_pairs[, 2] %in% paths_to_remove[i, 1]),
          which(node_pairs[, 1] %in% paths_to_remove[i, 2]))

      path_to_remove <- c(edge_1, edge_2)

      if (length(path_to_remove == 1)){
        node_pairs <- node_pairs[-path_to_remove, ]
      }
    }

    x = x + 1

    if (x == length(get_nodes(graph))){
      break
    }
  }

  # Move through remaining nodes and determine
  # whether each has a reachable set of nodes equal
  # to the number of nodes in the graph minus one;
  # the first node to not satisfy this condition
  # signifies an unconnected graph
  for (i in 1:length(unique(c(node_pairs[,1],
                              node_pairs[,2])))) {

    if (length(get_all_connected_nodes(
      graph = graph,
      node = unique(c(node_pairs[,1],
                      node_pairs[,2]))[i])) !=
      node_count(graph) - 1) {
      return(FALSE)
    }
  }

  return(TRUE)
}
