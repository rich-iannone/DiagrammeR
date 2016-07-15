#' Reverse the graph's edge direction
#' @description Take a directed graph and reverse the
#' edge directions.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a graph with a directed tree
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2)
#'
#' # Reverse the edge directions such that edges
#' # are directed toward the root of the tree
#' graph <-
#'   graph %>% reverse_edge_direction
#'
#' # Inspect the graph's edges
#' graph %>% get_edges
#' #> [1] "2 -> 1" "3 -> 1" "4 -> 2" "5 -> 2" "6 -> 3"
#' #> [6] "7 -> 3"
#' @export reverse_edge_direction

reverse_edge_direction <- function(graph) {

  # If graph is undirected, stop function
  if (graph$directed == FALSE) {
    stop("The input graph must be a directed graph.")
  }

  # Get the graph nodes in the `from` and `to` columns
  # of the edf
  from <- get_edges(graph, return_type = "df")[, 1]
  to <- get_edges(graph, return_type = "df")[, 2]

  # Extract the graph's edge data frame
  edges <- get_edge_df(graph)

  # Replace the contents of the `from` and `to` columns
  edges$from <- to
  edges$to <- from

  # Create a new graph object, using the `edges` edf
  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = edges,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = TRUE,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
