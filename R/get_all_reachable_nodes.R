#' Get all nodes reachable from a starting node
#' @description With a single node serving as
#' the starting point get all nodes reachable (i.e.,
#' with a traversible path) to that starting node.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node a single-length vector containing a
#' node ID value.
#' @return a vector of node ID values.
#' @export get_all_reachable_nodes

get_all_reachable_nodes <- function(graph,
                                    node) {

  # Create an empty list object
  nodes <- list()

  # Get a vector of all nodes in the graph
  graph_nodes <- get_nodes(graph)

  # place starting node in the `reachable`` vector
  reachable <- node

  # Initialize `i`
  i <- 1

  repeat {

    # From the starting node get all adjacent nodes
    # that are not in the `reachable` vector
    reachable <-
      unique(
        c(reachable,
          intersect(
            unique(c(
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 1] %in%
                      reachable), 2],
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 2] %in%
                      reachable), 1])),
            graph_nodes)))

    # Place reachable nodes in `nodes` list
    nodes[[i]] <- reachable

    # Break if current iteration yields no change in
    # the `nodes` list
    if (i > 1){
      if (identical(nodes[[i]], nodes[[i - 1]])) break
    }

    i <- i + 1
  }

  # If there are no nodes in the `reachable` vector`,
  # return NA
  if (length(reachable) == 0) {
    return(NA)
  }

  # Remove the starting node from the `reachable`
  # vector to get the neighbors of the starting node
  reachable <-
    setdiff(reachable, node)

  # Determine if the node ID values in the
  # `reachable` vector are numeric
  node_id_numeric <-
    ifelse(
      suppressWarnings(
        any(is.na(as.numeric(reachable)))),
      FALSE, TRUE)

  # If the node ID values are numeric, then apply a
  # numeric sort and reclass as a `character` type
  if (node_id_numeric) {
    reachable <-
      as.character(sort(as.numeric(reachable)))
  }

  return(reachable)
}
