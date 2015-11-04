#' Create a randomized graph
#' @description Create a graph of up to \code{n} nodes with randomized edge
#' assignments.
#' @param n the number of nodes to use in the random graph.
#' @param m the number of edges to use in the random graph.
#' @param directed an option for whether the random graph should be
#' undirected (default) or directed.
#' @param fully_connected should the graph be fully connected (i.e. no
#' free nodes).
#' @param display_labels display node labels.
#' @param set_seed supplying a value sets a random seed of the
#'\code{Mersenne-Twister} implementation.
#' @param node_id an optional vector of unique node ID values to apply to the
#' randomized graph. The length of the vector should ideally correspond to
#' the value supplied in \code{n}; vectors longer than the length of \code{n}
#' will be truncated.
#' @examples
#' \dontrun{
#' # Create a random, directed graph with 50 nodes and 75 edges
#' random_graph_directed <-
#'   create_random_graph(50, 75, directed = TRUE)
#'
#' # Create a random, undirected graph that's fully connected
#' random_graph_undirected <-
#'   create_random_graph(30, 30, fully_connected = TRUE)
#'
#' # Create a directed graph with a seed set so that it's reproducible
#' directed_graph <-
#'   create_random_graph(15, 34, set_seed = 50)
#'
#' # Create a directed, random graph with a supplied set of node IDs
#' random_directed_graph_letters <-
#'   create_random_graph(n = 10, m = 20,
#'                       directed = TRUE,
#'                       node_id = LETTERS)
#' }
#' @export create_random_graph

create_random_graph <- function(n,
                                m,
                                directed = FALSE,
                                fully_connected = FALSE,
                                display_labels = TRUE,
                                set_seed = NULL,
                                node_id = NULL){

  if (!is.null(set_seed)) set.seed(set_seed, kind = "Mersenne-Twister")

  # Stop function if the number of edges requested exceeds that of a
  # fully connected graph
  if (m > n * ((n - 1)/ 2)){
    stop(paste0("The number of edges exceeds the maximum possible (",
                n * ((n - 1)/ 2),
                ")"))
  }

  if (!is.null(node_id)){

    # Stop function if all node ID values are not unique
    if (anyDuplicated(node_id) != 0){
      stop("The supplied node IDs are not unique.")
    }

    # Stop function if insufficient node IDs values provided
    if (length(node_id) < n){
      stop("Not enough node ID values were provided.")
    }

    graph <-
      create_graph(nodes_df =
                     create_nodes(nodes = node_id[1:n],
                                  label = ifelse(display_labels == TRUE,
                                                 TRUE, FALSE),
                                  value = sample(seq(0.5, 10, 0.5),
                                                 n, replace = TRUE)),
                   directed = ifelse(directed == TRUE, TRUE, FALSE))
  } else {

    graph <-
      create_graph(nodes_df =
                     create_nodes(nodes = 1:n,
                                  label = ifelse(display_labels == TRUE,
                                                 TRUE, FALSE),
                                  value = sample(seq(0.5, 10, 0.5),
                                                 n, replace = TRUE)),
                   directed = ifelse(directed == TRUE, TRUE, FALSE))
  }

  if (m > 0){
    for (i in 1:m){

      edge_placed <- FALSE

      while (edge_placed == FALSE) {

        edge_placed <- FALSE

        node_a <-
          sample(get_nodes(graph), 1)

        node_b <-
          sample(get_nodes(graph)[-which(get_nodes(graph) %in% node_a)], 1)

        edge_in_graph <-
          edge_present(graph,
                       from = node_a,
                       to = node_b) |
          edge_present(graph,
                       from = node_b,
                       to = node_a)

        if (edge_in_graph == FALSE){
          graph <- add_edge(graph,
                            from = node_a,
                            to = node_b)

          edge_placed <- TRUE
        }
      }
    }
  }

  if (fully_connected == TRUE){

    repeat {

      if (any(node_info(graph)$degree == 0)){

        unconnected_nodes <-
          node_info(graph)[which(node_info(graph)$degree == 0),1]

        connected_nodes <-
          setdiff(get_nodes(graph), unconnected_nodes)

        graph <-
          add_edge(graph,
                   from = sample(unconnected_nodes, 1),
                   to = sample(connected_nodes, 1))
      } else { break }
    }
  }

  return(graph)
}
