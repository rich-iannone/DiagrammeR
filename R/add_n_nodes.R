#' Add one or several unconnected nodes to the graph
#' @description Add n new nodes to a graph object of
#' class \code{dgr_graph}. Optionally, set node
#' \code{type} values for the new nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of new nodes to add to the graph.
#' @param type an optional string to apply a
#' \code{type} attribute to all newly created nodes.
#' @param label a character object for supplying an
#' optional label to the node. Setting to \code{TRUE}
#' ascribes the node ID to the label. Setting to
#' \code{FALSE} yields a blank label.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 5 nodes; these
#' # nodes will be assigned ID values from `1` to `5`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(5)
#'
#' # Get the graph's nodes
#' graph %>% get_nodes
#' #> [1] 1 2 3 4 5
#' @export add_n_nodes

add_n_nodes <- function(graph,
                        n,
                        type = NULL,
                        label = TRUE) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  if (!is.null(type)) {
    new_nodes <-
      create_nodes(
        nodes = seq(nodes_created + 1,
                    nodes_created + n,
                    1),
        label = label,
        type = type)
  } else {
    new_nodes <-
      create_nodes(
        nodes = seq(nodes_created + 1,
                    nodes_created + n,
                    1),
        label = label)
  }

  graph <- add_node_df(graph, new_nodes)

  # Update the `last_node` counter
  graph$last_node <- nodes_created + n

  return(graph)
}
