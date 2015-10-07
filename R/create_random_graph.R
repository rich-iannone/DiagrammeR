#' Create a randomized graph
#' @description Create a graph of up to \code{n} nodes with randomized edge
#' assignments.
#' @param n the number of nodes to use in the random graph.
#' @param m the number of edges to use in the random graph.
#' @examples
#' \dontrun{
#' # Create a random graph with 50 nodes
#' graph_random <- create_random_graph(50)
#' }
#' @export create_random_graph

create_random_graph <- function(n, m){

  # Stop function if the number of edges requested exceeds that of a
  # fully connected graph
  if (m > n * ((n - 1)/ 2)){

    stop(paste0("The number of edges exceeds the maximum possible (",
                n * ((n - 1)/ 2),
                ")"))
  }

  graph <-

  return(graph)
}
