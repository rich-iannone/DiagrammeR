#' Get total degree values for all nodes
#' @description Get the total degree values for all
#' nodes in a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param normalized set as \code{FALSE} (the default),
#' the total degree will be provided for each of
#' the nodes (as a count of edges to and from each
#' node). When set as \code{TRUE}, then the result for
#' each node will be divided by the total number of nodes
#' in the graph minus 1.
#' @return a data frame with total degree values for
#' each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 23)
#'
#' # Get the total degree values for all nodes
#' # in the graph
#' get_degree_total(graph)
#' #>    id total_degree
#' #> 1   1            4
#' #> 2   2            5
#' #> 3   3            4
#' #> 4   4            3
#' #> 5   5            5
#' #> 6   6            7
#' #> 7   7            4
#' #> 8   8            2
#' #> 9   9            5
#' #> 10 10            5
#'
#' # Add the total degree values to the graph
#' # as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     get_degree_total(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value total_degree
#' #> 1   1 <NA>     1   6.0            4
#' #> 2   2 <NA>     2   2.5            5
#' #> 3   3 <NA>     3   3.5            4
#' #> 4   4 <NA>     4   7.5            3
#' #> 5   5 <NA>     5   8.5            5
#' #> 6   6 <NA>     6   4.5            7
#' #> 7   7 <NA>     7  10.0            4
#' #> 8   8 <NA>     8  10.0            2
#' #> 9   9 <NA>     9   8.5            5
#' #> 10 10 <NA>    10  10.0            5
#' @importFrom igraph degree
#' @export get_degree_total

get_degree_total <- function(graph,
                             normalized = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the total degree values for each of the
  # graph's nodes
  if (normalized == TRUE) {
    total_degree_values <-
      igraph::degree(
        ig_graph,
        mode = "total",
        normalized = TRUE)
  }

  if (normalized == FALSE) {
    total_degree_values <-
      igraph::degree(
        ig_graph,
        mode = "total",
        normalized = FALSE)
  }

  # Create df with total degree scores
  total_degree_values_df <-
    data.frame(
      id = names(total_degree_values),
      total_degree = total_degree_values,
      stringsAsFactors = FALSE)

  return(total_degree_values_df)
}
