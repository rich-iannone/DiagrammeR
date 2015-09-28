#' Create a randomized graph
#' @description Create a graph of up to \code{n} nodes with randomized edge
#' assignments.
#' @param n the number of nodes to use in the random graph.
#' @examples
#' \dontrun{
#' # Create a random graph with 50 nodes
#' graph_random <- create_random_graph(50)
#' }
#' @export create_random_graph

create_random_graph <- function(n){

  edges <-
    create_edges(from = sample(1:n, n, replace = TRUE),
                 to = sample(1:n, n, replace = TRUE))

  nodes <-
    create_nodes(nodes = 1:n,
                 value = sample(seq(0.5, 10, 0.5), n, replace = TRUE))

  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges)

  return(graph)
}
