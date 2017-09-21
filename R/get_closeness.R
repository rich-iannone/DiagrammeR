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
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 12,
#'     set_seed = 23)
#'
#' # Get closeness values for all nodes
#' # in the graph
#' get_closeness(graph)
#' #>    id  closeness
#' #> 1   1 0.05263158
#' #> 2   2 0.06250000
#' #> 3   3 0.05555556
#' #> 4   4 0.06666667
#' #> 5   5 0.06666667
#' #> 6   6 0.04166667
#' #> 7   7 0.04347826
#' #> 8   8 0.05555556
#' #> 9   9 0.04166667
#' #> 10 10 0.04545455
#'
#' # Add the closeness values to
#' # the graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_closeness(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label  closeness
#' #> 1   1 <NA>  <NA> 0.05263158
#' #> 2   2 <NA>  <NA> 0.06250000
#' #> 3   3 <NA>  <NA> 0.05555556
#' #> 4   4 <NA>  <NA> 0.06666667
#' #> 5   5 <NA>  <NA> 0.06666667
#' #> 6   6 <NA>  <NA> 0.04166667
#' #> 7   7 <NA>  <NA> 0.04347826
#' #> 8   8 <NA>  <NA> 0.05555556
#' #> 9   9 <NA>  <NA> 0.04166667
#' #> 10 10 <NA>  <NA> 0.04545455
#' @importFrom igraph closeness
#' @export get_closeness

get_closeness <- function(graph,
                          direction = "all") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  if (!(direction %in% c("all", "in", "out"))) {
    stop("Valid options for `direction` are `all`, `in`, or `out`.")
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
  data.frame(
    id = closeness_values %>%
      names() %>%
      as.integer(),
    closeness = closeness_values,
    stringsAsFactors = FALSE)
}
