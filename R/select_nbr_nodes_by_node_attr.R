#' Select neighboring nodes based on node attribute
#' similarity
#' @description Beginning with a selection of a single
#' node, select those nodes in a neighborhood
#' of nodes that have a common node attribute.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr the name of the node attribute
#' to use to compare with adjacent nodes.
#' @param match the value of the node attribute to
#' match on.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_nbr_nodes_by_node_attr

select_nbr_nodes_by_node_attr <- function(graph,
                                          node_attr,
                                          match) {

  # Create an empty list object
  nodes <- list()

  # Get the set of all nodes in graph that
  # satisfy the condition
  graph_nodes_with_attr <-
    graph$nodes_df[
      which(
        graph$nodes_df[, which(
          colnames(graph$nodes_df) ==
            node_attr)] == match), 1]

  # place starting node in the neighbourhood vector
  neighborhood <- get_selection(graph)[[1]]

  # Initialize `i`
  i <- 1

  repeat {

    # From the starting node get all adjacent nodes
    # that are not in the `neighborhood` vector
    neighborhood <-
      unique(
        c(neighborhood,
          intersect(
            unique(c(
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 1] %in%
                      neighborhood), 2],
              get_edges(
                graph,
                return_type = "df")[
                  which(get_edges(
                    graph,
                    return_type = "df")[, 2] %in%
                      neighborhood), 1])),
            graph_nodes_with_attr)))

    # Place revised neighborhood nodes in `nodes` list
    nodes[[i]] <- neighborhood

    # Break if current iteration yields no change in
    # the `nodes` list
    if (i > 1){
      if (identical(nodes[[i]], nodes[[i - 1]])) break
    }

    i <- i + 1
  }

  # Get the final set of nodes that satisfy similarity
  # and adjacency conditions
  matching_nodes <- nodes[length(nodes)][[1]]

  # Replace the graph selection with the matched nodes
  graph$selection$nodes <- matching_nodes

  return(graph)
}
