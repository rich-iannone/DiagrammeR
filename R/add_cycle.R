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
#' #>   id type label deg indeg outdeg loops
#' #> 1  1 <NA>     1   2     1      1     0
#' #> 2  2 <NA>     2   2     1      1     0
#' #> 3  3 <NA>     3   2     1      1     0
#' #> 4  4 <NA>     4   2     1      1     0
#' #> 5  5 <NA>     5   2     1      1     0
#' #> 6  6 <NA>     6   2     1      1     0
#' @export add_cycle

add_cycle <- function(graph,
                      n,
                      type = NULL,
                      label = TRUE,
                      rel = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop if n is too small
  if (n <= 2)  {
    stop("The value for n must be at least 3.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the sequence of nodes required
  nodes <- seq(1, n)

  # Create a node data frame for the tree graph
  cycle_nodes <-
    create_node_df(
      n = n,
      type = type,
      label = label)

  # Create an edge data frame for the cycle graph
  cycle_edges <-
    create_edge_df(
      from = nodes,
      to = c(nodes[2:length(nodes)], nodes[1]),
      rel = rel)

  # Create the cycle graph
  cycle_graph <- create_graph(cycle_nodes, cycle_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, cycle_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(cycle_nodes)

    return(combined_graph)
  } else {
    return(cycle_graph)
  }
}
