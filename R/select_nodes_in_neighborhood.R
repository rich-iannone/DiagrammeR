#' Select nodes based on a walk distance from a
#' specified node
#' @description Select those nodes in the neighborhood
#' of nodes connected a specified distance from an
#' initial node.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node the node from which the traversal
#' will originate.
#' @param distance the maximum number of steps from
#' the \code{node} for inclusion in the selection.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a graph with a tree structure that's
#' # 4 levels deep (begins with node `1`, branching
#' # by 3 nodes at each level); the resulting graph
#' # contains 40 nodes, numbered `1` through `40`
#' graph <-
#'   create_graph(graph_attrs = 'layout = twopi') %>%
#'   add_node("A") %>% select_nodes %>%
#'   add_n_nodes_ws(3, "from", "B") %>%
#'   clear_selection %>%
#'   select_nodes("type", "B") %>%
#'   add_n_nodes_ws(3, "from", "C") %>%
#'   clear_selection %>%
#'   select_nodes("type", "C") %>%
#'   add_n_nodes_ws(3, "from", "D") %>%
#'   clear_selection
#'
#' # Create a graph selection by selecting nodes
#' # in the neighborhood of node `1`, where the
#' # neighborhood is limited by nodes that are 1
#' # connection away from node `1`
#' graph %<>%
#'   select_nodes_in_neighborhood(
#'     node = 1,
#'     distance = 1)
#'
#' # Get the selection of nodes
#' graph %>% get_selection
#' #> $nodes
#' #> [1] "1" "2" "3" "4"
#'
#' # Perform another selection of nodes, this time
#' # with a neighborhood spanning 2 nodes from node `1`
#' graph %<>%
#'   clear_selection %>%
#'   select_nodes_in_neighborhood(
#'     node = 1,
#'     distance = 2)
#'
#' # Get the selection of nodes
#' graph %>% get_selection
#' #> $nodes
#' #> [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"
#' #> [10] "10" "11" "12" "13"
#'
#' # Perform a final selection of nodes, using
#' # `distance = 3`, this effectively selects all 40
#' # nodes in this graph
#' graph %<>%
#'   clear_selection %>%
#'   select_nodes_in_neighborhood(
#'     node = 1,
#'     distance = 3)
#'
#' # Get a count of the nodes selected to verify
#' # that all 40 nodes have indeed been selected
#' graph %>% get_selection %>% unlist %>% length
#' #> [1] 40
#' @export select_nodes_in_neighborhood

select_nodes_in_neighborhood <- function(graph,
                                         node,
                                         distance,
                                         set_op = "union") {

  # Create an empty list object
  nodes <- list()

  # Find nodes belonging to the neighborhood
  for (i in 1:distance) {
    if (i == 1) {

      nodes[[i]] <-
        vector(mode = "character")

      nodes[[i]] <-
        c(node,
          as.character(
            get_edges(
              graph,
              return_type = "df")[
                which(
                  get_edges(
                    graph,
                    return_type = "df")[, 1] ==
                    node), 2]),
          as.character(
            get_edges(
              graph,
              return_type = "df")[
                which(
                  get_edges(
                    graph,
                    return_type = "df")[, 2] ==
                    node), 1]))
    }

    if (i > 1) {
      for (j in 1:length(nodes[[i - 1]])) {
        if (j == 1) {
          nodes[[i]] <- vector(mode = "character")
        }

        nodes[[i]] <-
          c(nodes[[i]],
            as.character(
              get_edges(
                graph,
                return_type = "df")[
                  which(
                    get_edges(
                      graph,
                      return_type = "df")[, 1] ==
                      nodes[[i - 1]][j]), 2]),
            as.character(
              get_edges(
                graph,
                return_type = "df")[
                  which(
                    get_edges(
                      graph,
                      return_type = "df")[, 2] ==
                      nodes[[i - 1]][j]), 1]))
      }
    }
  }

  # From list of nodes, obtain vector of unique
  # nodes as neighbors
  nodes_selected <- unique(unlist(nodes))

  # Obtain vector of node IDs selection of nodes
  # already present
  if (!is.null(graph$selection)) {
    if (!is.null(graph$selection$nodes)) {
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected nodes into graph's selection
  if (set_op == "union") {
    nodes_combined <-
      union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <-
      intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <-
      setdiff(nodes_prev_selection, nodes_selected)
  }

  graph$selection$nodes <- nodes_combined

  return(graph)
}
