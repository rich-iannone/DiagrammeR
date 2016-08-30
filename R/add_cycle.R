#' Add a cycle of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node cycle to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of nodes comprising the cycle.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node cycle.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add a cycle of nodes to it
#' graph <-
#'   create_graph() %>%
#'   add_cycle(6)
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>   node label type deg indeg outdeg loops
#' #> 1    1     1        2     1      1     0
#' #> 2    2     2        2     1      1     0
#' #> 3    3     3        2     1      1     0
#' #> 4    4     4        2     1      1     0
#' #> 5    5     5        2     1      1     0
#' #> 6    6     6        2     1      1     0
#' @export add_cycle

add_cycle <- function(graph,
                      n,
                      type = NULL,
                      label = TRUE,
                      rel = NULL) {

  # Stop if n is too small
  if (n <= 2)  {
    stop("The value for n must be at least 3.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  nodes <-
    seq(nodes_created + 1,
        nodes_created + n)

  cycle_nodes <-
    create_nodes(
      nodes = nodes,
      type = type,
      label = label)

  graph <-
    add_node_df(graph, cycle_nodes)

  cycle_edges <-
    create_edges(
      from = nodes,
      to = c(nodes[2:length(nodes)], nodes[1]),
      rel = rel)

  graph <- add_edge_df(graph, cycle_edges)

  # Update the `last_node` counter
  graph$last_node <- nodes_created + nrow(cycle_nodes)

  return(graph)
}
