#' Get all nodes connected to a specified node
#' @description With a single node serving as
#' the starting point get all nodes connected (i.e.,
#' with a traversible path) to that node.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node a single-length vector containing a
#' node ID value.
#' @return a vector of node ID values.
#' @export get_all_connected_nodes

get_all_connected_nodes <- function(graph,
                                    node) {

  # Create an empty list object
  nodes <- list()

  # Get a vector of all nodes in the graph
  graph_nodes <- get_nodes(graph)

  # place starting node in the `connected`` vector
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

    # Break if current iteration yields no change in
    # the `nodes` list
    if (i > 1){
      if (identical(nodes[[i]], nodes[[i - 1]])) break
    }

    i <- i + 1
  }

  # If there are no nodes in the `connected` vector`,
  # return NA
  if (length(connected) == 0) {
    return(NA)
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
