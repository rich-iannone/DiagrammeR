#' Add a multiple of new nodes with edges to one or
#' more selected nodes
#' @description Add n new nodes to one or more nodes
#' available in a graph object of class
#' \code{dgr_graph}, with edges moving to the nodes in
#' the selection from the newly created nodes.
#' Optionally, set node \code{type} and edge
#' \code{rel} values for the new nodes and edges.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of new nodes to attach as
#' predecessor nodes to the nodes in the selection.
#' @param set_node_type an optional string to apply a
#' \code{type} attribute to all newly created nodes.
#' @param set_edge_rel an optional string to apply a
#' \code{rel} attribute to all newly created edges.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create an empty graph, add a node to it, select
#' # that node, and 5 more nodes to the graph with
#' # edges from the new nodes to the original node
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(1) %>%
#'   select_last_node %>%
#'   add_n_nodes_to_selection(5)
#'
#' # Get the graph's nodes
#' graph %>% get_nodes
#' #> [1] "1" "2" "3" "4" "5" "6"
#'
#' # Get the graph's edges
#' graph %>% get_edges(return_type = "vector")
#' #> [1] "2 -> 1" "3 -> 1" "4 -> 1" "5 -> 1" "6 -> 1"
#' }
#' @export add_n_nodes_to_selection

add_n_nodes_to_selection <- function(graph,
                                     n,
                                     set_node_type = NULL,
                                     set_edge_rel = NULL) {

  # If no node selection is available, return
  # the graph unchanged
  if (is.null(graph$selection$nodes)) {
    return(graph)
  }

  # Get a vector of nodes available in the
  # graph's selection
  nodes_in_selection <- graph$selection$nodes

  for (i in 1:length(nodes_in_selection)) {

    if (node_count(graph) == 0){
      node <- 1
    }

    if (node_count(graph) > 0){
      if (!is.na(
        suppressWarnings(
          any(as.numeric(get_nodes(graph)))))){

        numeric_components <-
          suppressWarnings(
            which(
              !is.na(as.numeric(get_nodes(graph)))))

        node <-
          max(
            as.integer(
              as.numeric(
                get_nodes(graph)[
                  numeric_components]))) + 1
      }

      if (suppressWarnings(
        all(
          is.na(as.numeric(get_nodes(graph)))))){
        node <- 1
      }
    }

    if (!is.null(set_node_type)) {
      new_nodes <-
        create_nodes(
          nodes = seq(node, node + n - 1, 1),
          type = set_node_type)
    } else {
      new_nodes <-
        create_nodes(
          nodes = seq(node, node + n - 1, 1))
    }

    if (!is.null(set_edge_rel)) {
      new_edges <-
        create_edges(
          from = seq(node, node + n - 1, 1),
          to = rep(nodes_in_selection[i], n),
          rel = set_edge_rel)
    } else {
      new_edges <-
        create_edges(
          from = seq(node, node + n - 1, 1),
          to = rep(nodes_in_selection[i], n))
    }

    graph <-
      add_node_df(graph, new_nodes)

    graph <-
      add_edge_df(graph, new_edges)

    graph$selection$nodes <- nodes_in_selection
  }

  return(graph)
}
