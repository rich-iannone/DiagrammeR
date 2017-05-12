#' Get indegree values for all nodes
#' @description Get the indegree values for all
#' nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param normalized set as \code{FALSE} (the default),
#' the indegree will be provided for each of
#' the nodes (as a count of edges to each node). When set
#' as \code{TRUE}, then the result for each node will be
#' divided by the total number of nodes in the graph minus 1.
#' @return a data frame with indegree values for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 23)
#'
#' # Get the indegree values for all nodes
#' # in the graph
#' get_degree_in(graph)
#' #>    id indegree
#' #> 1   1        0
#' #> 2   2        0
#' #> 3   3        1
#' #> 4   4        0
#' #> 5   5        3
#' #> 6   6        4
#' #> 7   7        3
#' #> 8   8        2
#' #> 9   9        4
#' #> 10 10        5
#'
#' # Add the indegree values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     get_degree_in(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value indegree
#' #> 1   1 <NA>     1   6.0        0
#' #> 2   2 <NA>     2   2.5        0
#' #> 3   3 <NA>     3   3.5        1
#' #> 4   4 <NA>     4   7.5        0
#' #> 5   5 <NA>     5   8.5        3
#' #> 6   6 <NA>     6   4.5        4
#' #> 7   7 <NA>     7  10.0        3
#' #> 8   8 <NA>     8  10.0        2
#' #> 9   9 <NA>     9   8.5        4
#' #> 10 10 <NA>    10  10.0        5
#' @importFrom igraph degree
#' @export get_degree_in

get_degree_in <- function(graph,
                          normalized = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the indegree values for each of the
  # graph's nodes
  if (normalized == TRUE) {
    indegree_values <-
      igraph::degree(
        ig_graph,
        mode = "in",
        normalized = TRUE)
  }

  if (normalized == FALSE) {
    indegree_values <-
      igraph::degree(
        ig_graph,
        mode = "in",
        normalized = FALSE)
  }

  # Create df with indegree scores
  indegree_values_df <-
    data.frame(
      id = names(indegree_values),
      indegree = indegree_values,
      stringsAsFactors = FALSE)

  return(indegree_values_df)
}
