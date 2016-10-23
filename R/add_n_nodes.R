#' Add one or several unconnected nodes to the graph
#' @description Add n new nodes to a graph object of
#' class \code{dgr_graph}. Optionally, set node
#' \code{type} values for the new nodes.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of new nodes to add to the graph.
#' @param type an optional character vector that
#' provides group identifiers for the nodes to be added.
#' @param label an optional character object that
#' describes the nodes to be added.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 5 nodes; these
#' # nodes will be assigned ID values from `1` to `5`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(5)
#'
#' # Get the graph's nodes
#' graph %>% get_node_ids
#' #> [1] 1 2 3 4 5
#' @importFrom dplyr bind_rows
#' @export add_n_nodes

add_n_nodes <- function(graph,
                        n,
                        type = NULL,
                        label = NULL) {

  if (is.null(type)) {
    type <- as.character(NA)
  }

  if (is.null(label)) {
    label <- as.character(NA)
  }

  # Create a ndf of the correct length
  new_nodes <-
    create_node_df(
      n = n,
      type = type,
      label = label)

  new_nodes[, 1] <- new_nodes[, 1] + graph$last_node

  graph$nodes_df <-
    dplyr::bind_rows(graph$nodes_df, new_nodes)

  # Update the `last_node` counter
  graph$last_node <- graph$last_node + n

  return(graph)
}
