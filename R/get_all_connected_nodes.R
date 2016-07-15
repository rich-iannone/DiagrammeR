#' Get all nodes connected to a specified node
#' @description With a single node serving as
#' the starting point get all nodes connected (i.e.,
#' reachable with a traversible path) to that node.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node a single-length vector containing a
#' node ID value.
#' @return a vector of node ID values.
#' @examples
#' library(magrittr)
#'
#' # This graph, created using `create_random_graph()`
#' # is almost fully connected but there is an
#' # isolated node (`13`) with no edges
#' graph_1 <-
#'   create_random_graph(
#'     30, 50, set_seed = 1)
#'
#' # There won't be any connected nodes to `13` so when
#' # specifying this isolated node with
#' # `get_all_connected_nodes()` will return NA
#' graph_1 %>% get_all_connected_nodes(13)
#' #> [1] NA
#'
#' # Any other node in `graph_1` will provide a vector
#' # of all the nodes other than `13`
#' graph_1 %>% get_all_connected_nodes(2)
#' #> [1] "1"  "3"  "4"  "5"  "6"  "7"  "8"  "9"
#' #> [9] "10" "11" "12" "14" "15" "16" "17" "18"
#' #> [17] "19" "20" "21" "22" "23" "24" "25" "26"
#' #> [25] "27" "28" "29" "30"
#'
#' # The following graph is fully connected
#' graph_2 <-
#'   create_random_graph(
#'     36, 50, set_seed = 1)
#'
#' # The following graph has two clusters of nodes
#' # (i.e., the graph has two connected components)
#' graph_2 <-
#'   create_random_graph(
#'     36, 50, set_seed = 1) %>%
#'   delete_edge(10, 36) %>%
#'   delete_edge(25, 27) %>%
#'   delete_edge(28, 29) %>%
#'   delete_edge(4, 29) %>%
#'   delete_edge(24, 32)
#'
#' # In `graph_2`, node `1` is in the larger of the two
#' # connected components
#' graph_2 %>% get_all_connected_nodes(1)
#' #> [1] "3"  "4"  "5"  "6"  "7"  "8"  "9"  "11"
#' #> [9] "14" "16" "18" "19" "21" "22" "23" "25"
#' #> [17] "26" "28" "30" "32" "33" "34" "35" "36"
#'
#' # Also in `graph_2`, node `2` is in the smaller of
#' # the two connected components
#' graph_2 %>% get_all_connected_nodes(2)
#' #> [1] "10" "12" "13" "15" "17" "20" "24" "27"
#' #> [9] "29" "31"
#' @export get_all_connected_nodes

get_all_connected_nodes <- function(graph,
                                    node) {

  # Create an empty list object
  nodes <- list()

  # Get a vector of all nodes in the graph
  graph_nodes <- get_nodes(graph)

  # place starting node in the `connected` vector
  connected <- node

  # Initialize `i`
  i <- 1

  repeat {

    # From the starting node get all adjacent nodes
    # that are not in the `connected` vector
    connected <-
      unique(
        c(connected,
          intersect(
            unique(c(
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 1] %in%
                      connected), 2],
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 2] %in%
                      connected), 1])),
            graph_nodes)))

    # Place connected nodes in `nodes` list
    nodes[[i]] <- connected

    # If there is only the starting node in the
    # `connected` vector on the first pass, return NA
    if (i == 1 & length(connected) == 1) {
      if (connected == node) {
        return(NA)
      }
    }

    # Break if current iteration yields no change in
    # the `nodes` list
    if (i > 1){
      if (identical(nodes[[i]], nodes[[i - 1]])) break
    }

    i <- i + 1
  }

  # Remove the starting node from the `connected`
  # vector to get the neighbors of the starting node
  connected <-
    setdiff(connected, node)

  # Determine if the node ID values in the
  # `connected` vector are numeric
  node_id_numeric <-
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(connected)))),
      FALSE, TRUE)

  # If the node ID values are numeric, then apply a
  # numeric sort and reclass as a `character` type
  if (node_id_numeric) {
    connected <-
      as.character(sort(as.numeric(connected)))
  }

  return(connected)
}
