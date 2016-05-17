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

  # If no node selection is available, return the graph unchanged
  if (is.null(graph$selection$nodes)) {
    return(graph)
  }

  # Get a vector of nodes available in the graph's selection
  nodes_in_selection <- graph$selection$nodes

  # For all nodes in selection, create `n` successor nodes
  for (i in 1:length(nodes_in_selection)) {
    for (j in 1:n) {

      graph <-
        add_node(
          graph = graph,
          to = nodes_in_selection[i],
          label = FALSE)

      graph$selection$nodes <- nodes_in_selection

      # Apply node `type` value to all new edges, if supplied
      if (!is.null(set_node_type)) {

        graph <-
          select_last_node(graph = graph)

        graph <-
          set_node_attr_with_selection(
            graph = graph,
            node_attr = "type",
            value = set_node_type)

        graph$selection$nodes <- nodes_in_selection
      }

      # Apply edge `rel` value to all new edges, if supplied
      if (!is.null(set_edge_rel)) {

        graph <-
          select_last_edge(graph = graph)

        graph <-
          set_edge_attr_with_selection(
            graph = graph,
            edge_attr = "rel",
            value = set_edge_rel)

        graph$selection$nodes <- nodes_in_selection
      }
    }
  }

  return(graph)
}
