#' Get outdegree values for all nodes
#' @description Get the outdegree values for all
#' nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param normalized set as \code{FALSE} (the default),
#' the outdegree will be provided for each of
#' the nodes (as a count of edges outgoing from each node).
#' When set as \code{TRUE}, then the result for each node
#' will be divided by the total number of nodes in the
#' graph minus 1.
#' @return a data frame with outdegree values for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the outdegree values for all nodes
#' # in the graph
#' get_degree_out(graph)
#' #>    id outdegree
#' #> 1   1         4
#' #> 2   2         5
#' #> 3   3         3
#' #> 4   4         3
#' #> 5   5         2
#' #> 6   6         3
#' #> 7   7         1
#' #> 8   8         0
#' #> 9   9         1
#' #> 10 10         0
#'
#' # Add the outdegree values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_degree_out(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value outdegree
#' #> 1   1 <NA>     1   6.0         4
#' #> 2   2 <NA>     2   2.5         5
#' #> 3   3 <NA>     3   3.5         3
#' #> 4   4 <NA>     4   7.5         3
#' #> 5   5 <NA>     5   8.5         2
#' #> 6   6 <NA>     6   4.5         3
#' #> 7   7 <NA>     7  10.0         1
#' #> 8   8 <NA>     8  10.0         0
#' #> 9   9 <NA>     9   8.5         1
#' #> 10 10 <NA>    10  10.0         0
#' @importFrom igraph degree
#' @export get_degree_out

get_degree_out <- function(graph,
                           normalized = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the outdegree values for each of the
  # graph's nodes
  if (normalized == TRUE) {
    outdegree_values <-
      igraph::degree(
        ig_graph,
        mode = "out",
        normalized = TRUE)
  }

  if (normalized == FALSE) {
    outdegree_values <-
      igraph::degree(
        ig_graph,
        mode = "out",
        normalized = FALSE)
  }

  # Create df with outdegree scores
  outdegree_values_df <-
    data.frame(
      id = names(outdegree_values),
      outdegree = outdegree_values,
      stringsAsFactors = FALSE)

  return(outdegree_values_df)
}
