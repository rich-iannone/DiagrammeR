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
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get closeness values for all nodes in the graph
#' get_closeness(graph)
#' #>    node  closeness
#' #> 1     1 0.07142857
#' #> 2     2 0.07692308
#' #> 3     3 0.06666667
#' #> 4     4 0.07142857
#' #> 5     5 0.08333333
#' #> 6     6 0.05882353
#' #> 7     7 0.08333333
#' #> 8     8 0.07692308
#' #> 9     9 0.06666667
#' #> 10   10 0.05882353
#'
#' # Add the closeness values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     get_closeness(.),
#'     by_graph = "nodes",
#'     by_df = "node")
#' @importFrom igraph closeness
#' @export get_closeness

get_closeness <- function(graph,
                          direction = "all") {

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
    data.frame(node = names(closeness_values),
               closeness = closeness_values,
               stringsAsFactors = FALSE)

  return(closeness_values_df)
}
