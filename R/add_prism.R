#' Add a prism of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node prism to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of nodes describing the shape
#' of the prism. For example, the triangonal prism has
#' \code{n} equal to 3 and it is composed of 6 nodes
#' and 9 edges. For any n-gonal prism, the graph will
#' be generated with 2\code{n} nodes and 3\code{n}
#' edges.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node prism.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add 2 prisms
#' graph <-
#'   create_graph() %>%
#'   add_prism(3, "prism", "a") %>%
#'   add_prism(3, "prism", "b")
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>    node label  type deg indeg outdeg loops
#' #> 1     1     a prism   3     1      2     0
#' #> 2     2     a prism   3     1      2     0
#' #> 3     3     a prism   3     1      2     0
#' #> 4     4     a prism   3     2      1     0
#' #> 5     5     a prism   3     2      1     0
#' #> 6     6     a prism   3     2      1     0
#' #> 7     7     b prism   3     1      2     0
#' #> 8     8     b prism   3     1      2     0
#' #> 9     9     b prism   3     1      2     0
#' #> 10   10     b prism   3     2      1     0
#' #> 11   11     b prism   3     2      1     0
#' #> 12   12     b prism   3     2      1     0
#' @export add_prism

add_prism <- function(graph,
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

  # Get the sequence of nodes required
  nodes <- seq(1, 2 * n)

  # Create a node data frame for the prism graph
  prism_nodes <-
    create_nodes(
      nodes = nodes,
      type = type,
      label = label)

  # Create an edge data frame for the prism graph
  prism_edges <-
    create_edges(
      from = c(nodes[1:(length(nodes)/2)],
               nodes[((length(nodes)/2) + 1):length(nodes)],
               nodes[1:(length(nodes)/2)]),
      to = c(nodes[2:(length(nodes)/2)],
             nodes[1],
             nodes[((length(nodes)/2) + 2):length(nodes)],
             nodes[((length(nodes)/2) + 1)],
             nodes[1:(length(nodes)/2)] + n),
      rel = rel)

  # Create the prism graph
  prism_graph <- create_graph(prism_nodes, prism_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    graph <- combine_graphs(graph, prism_graph)

    # Update the `last_node` counter
    graph$last_node <- nodes_created + nrow(prism_nodes)

    return(graph)
  } else {
    return(prism_graph)
  }
}
