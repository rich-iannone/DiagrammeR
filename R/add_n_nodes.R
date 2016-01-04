#' Add a multiple of new nodes to the graph
#' @description Add n new nodes to a graph object of class \code{dgr_graph}.
#' Optionally, set node \code{type} values for the new nodes.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param n the number of new nodes to add to the graph.
#' @param set_node_type an optional string to apply a \code{type} attribute
#' to all newly created nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @export add_n_nodes

add_n_nodes <- function(graph,
                        n,
                        set_node_type = NULL){

  # Create `n` new nodes in the graph
  for (i in 1:n){

    graph <- add_node(graph = graph,
                      label = FALSE)

    # Apply node `type` value to all new nodes, if supplied
    if (!is.null(set_node_type)){

      graph <-
        select_last_node(graph = graph)

      graph <-
        set_node_attr_with_selection(graph = graph,
                                     node_attr = "type",
                                     value = set_node_type)
    }
  }

  return(graph)
}
