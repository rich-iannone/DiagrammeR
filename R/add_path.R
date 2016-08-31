#' Add a path of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node path to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of nodes comprising the path.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node path.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add 3 paths of varying
#' # lengths
#' graph <-
#'   create_graph() %>%
#'   add_path(4, "four_path") %>%
#'   add_path(5, "five_path") %>%
#'   add_path(6, "six_path")
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>    node label      type deg indeg outdeg loops
#' #> 1     1     1 four_path   1     0      1     0
#' #> 2     2     2 four_path   2     1      1     0
#' #> 3     3     3 four_path   2     1      1     0
#' #> 4     4     4 four_path   1     1      0     0
#' #> 5     5     5 five_path   1     0      1     0
#' #> 6     6     6 five_path   2     1      1     0
#' #> 7     7     7 five_path   2     1      1     0
#' #> 8     8     8 five_path   2     1      1     0
#' #> 9     9     9 five_path   1     1      0     0
#' #> 10   10    10  six_path   1     0      1     0
#' #> 11   11    11  six_path   2     1      1     0
#' #> 12   12    12  six_path   2     1      1     0
#' #> 13   13    13  six_path   2     1      1     0
#' #> 14   14    14  six_path   2     1      1     0
#' #> 15   15    15  six_path   1     1      0     0
#' @export add_path

add_path <- function(graph,
                     n,
                     type = NULL,
                     label = TRUE,
                     rel = NULL) {

  # Stop if n is too small
  if (n <= 1)  {
    stop("The value for n must be at least 2.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the sequence of nodes required
  nodes <- seq(1, n)

  # Create a node data frame for the path graph
  path_nodes <-
    create_nodes(
      nodes = nodes,
      type = type,
      label = label)

  # Create an edge data frame for the path graph
  path_edges <-
    create_edges(
      from = nodes[1:length(nodes) - 1],
      to = nodes[2:length(nodes)],
      rel = rel)

  # Create the path graph
  path_graph <- create_graph(path_nodes, path_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    graph <- combine_graphs(graph, path_graph)

    # Update the `last_node` counter
    graph$last_node <- nodes_created + nrow(path_nodes)

    return(graph)
  } else {
    return(path_graph)
  }
}
