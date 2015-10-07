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
    create_graph(nodes_df = create_nodes(nodes = 1:n,
                                         value = sample(seq(0.5, 10, 0.5),
                                                        n, replace = TRUE)),
                 directed = FALSE)

  if (m > 0){
    for (i in 1:m){

      edge_placed <- FALSE

      while (edge_placed == FALSE) {

        edge_placed <- FALSE

        node_a <- sample(seq(1, n, 1), 1)
        node_b <- sample(seq(1, n, 1)[-which(1:n %in% node_a)], 1)

        edge_in_graph <-
          edge_present(graph,
                       from = node_a,
                       to = node_b) |
          edge_present(graph,
                       from = node_b,
                       to = node_a)

        if (edge_in_graph == FALSE){
          graph <- add_edges(graph,
                             from = node_a,
                             to = node_b)

          edge_placed <- TRUE
        }
      }
    }
  }

  return(graph)
}
