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
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the outdegree values for
#' # all nodes in the graph
#' get_degree_out(graph)
#' #>    id outdegree
#' #> 1   1         3
#' #> 2   2         3
#' #> 3   3         2
#' #> 4   4         5
#' #> 5   5         4
#' #> 6   6         5
#' #> 7   7         3
#' #> 8   8         2
#' #> 9   9         3
#' #> 10 10         0
#'
#' # Add the outdegree values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_degree_out(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label outdegree
#' #> 1   1 <NA>  <NA>         3
#' #> 2   2 <NA>  <NA>         3
#' #> 3   3 <NA>  <NA>         2
#' #> 4   4 <NA>  <NA>         5
#' #> 5   5 <NA>  <NA>         4
#' #> 6   6 <NA>  <NA>         5
#' #> 7   7 <NA>  <NA>         3
#' #> 8   8 <NA>  <NA>         2
#' #> 9   9 <NA>  <NA>         3
#' #> 10 10 <NA>  <NA>         0
#' @importFrom igraph degree
#' @export get_degree_out

get_degree_out <- function(graph,
                           normalized = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
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
  data.frame(
    id = outdegree_values %>%
      names() %>%
      as.integer(),
    outdegree = outdegree_values,
    stringsAsFactors = FALSE)
}
