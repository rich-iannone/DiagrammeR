#' Get radiality centrality scores
#' @description Get the radiality centrality
#' for all nodes in a graph. These scores describe
#' the ease to which nodes can reach other nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param direction using \code{all} (the default), the
#' search will ignore edge direction while traversing
#' through the graph. With \code{out}, measurements of
#' paths will be from a node whereas with \code{in},
#' measurements of paths will be to a node.
#' @return a data frame with radiality centrality
#' scores for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the radiality scores for nodes in the graph
#' get_radiality(graph)
#' #>    id radiality
#' #> 1   1  2.888889
#' #> 2   2  2.888889
#' #> 3   3  2.888889
#' #> 4   4  2.666667
#' #> 5   5  3.000000
#' #> 6   6  3.222222
#' #> 7   7  2.777778
#' #> 8   8  2.555556
#' #> 9   9  3.000000
#' #> 10 10  3.000000
#'
#' # Add the radiality values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_radiality(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value radiality
#' #> 1   1 <NA>     1   6.0  2.888889
#' #> 2   2 <NA>     2   2.5  2.888889
#' #> 3   3 <NA>     3   3.5  2.888889
#' #> 4   4 <NA>     4   7.5  2.666667
#' #> 5   5 <NA>     5   8.5  3.000000
#' #> 6   6 <NA>     6   4.5  3.222222
#' #> 7   7 <NA>     7  10.0  2.777778
#' #> 8   8 <NA>     8  10.0  2.555556
#' #> 9   9 <NA>     9   8.5  3.000000
#' #> 10 10 <NA>    10  10.0  3.000000
#' @importFrom CePa radiality
#' @importFrom igraph shortest.paths
#' @export get_radiality

get_radiality <- function(graph,
                          direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the radiality scores for each of the
  # graph's nodes
  if (direction == "all") {
    radiality_values <-
      CePa::radiality(ig_graph, mode = "all")
  }

  if (direction == "out") {
    radiality_values <-
      CePa::radiality(ig_graph, mode = "out")
  }

  if (direction == "in") {
    radiality_values <-
      CePa::radiality(ig_graph, mode = "in")
  }

  # Create df with radiality scores
  data.frame(
    id = names(radiality_values),
    radiality = radiality_values,
    stringsAsFactors = FALSE)
}
