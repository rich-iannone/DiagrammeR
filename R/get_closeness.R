#' Get closeness centrality values
#' @description Get the closeness centrality values
#' for all nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param direction using \code{all} (the default), the
#' search will ignore edge direction while traversing
#' through the graph. With \code{out}, measurements of
#' paths will be from a node whereas with \code{in},
#' measurements of paths will be to a node.
#' @return a data frame with closeness values for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get closeness values for all nodes
#' # in the graph
#' get_closeness(graph)
#' #>    id  closeness
#' #> 1   1 0.07142857
#' #> 2   2 0.07142857
#' #> 3   3 0.07142857
#' #> 4   4 0.06250000
#' #> 5   5 0.07692308
#' #> 6   6 0.09090909
#' #> 7   7 0.06666667
#' #> 8   8 0.05882353
#' #> 9   9 0.07692308
#' #> 10 10 0.07692308
#'
#' # Add the closeness values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_closeness(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value  closeness
#' #> 1   1 <NA>     1   6.0 0.07142857
#' #> 2   2 <NA>     2   2.5 0.07142857
#' #> 3   3 <NA>     3   3.5 0.07142857
#' #> 4   4 <NA>     4   7.5 0.06250000
#' #> 5   5 <NA>     5   8.5 0.07692308
#' #> 6   6 <NA>     6   4.5 0.09090909
#' #> 7   7 <NA>     7  10.0 0.06666667
#' #> 8   8 <NA>     8  10.0 0.05882353
#' #> 9   9 <NA>     9   8.5 0.07692308
#' #> 10 10 <NA>    10  10.0 0.07692308
#' @importFrom igraph closeness
#' @export get_closeness

get_closeness <- function(graph,
                          direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the betweenness scores for each of the
  # graph's nodes
  if (direction == "all") {
    closeness_values <-
      igraph::closeness(ig_graph, mode = "all")
  }

  if (direction == "out") {
    closeness_values <-
      igraph::closeness(ig_graph, mode = "out")
  }

  if (direction == "in") {
    closeness_values <-
      igraph::closeness(ig_graph, mode = "in")
  }

  # Create df with betweenness scores
  closeness_values_df <-
    data.frame(
      id = names(closeness_values),
      closeness = closeness_values,
      stringsAsFactors = FALSE)

  return(closeness_values_df)
}
