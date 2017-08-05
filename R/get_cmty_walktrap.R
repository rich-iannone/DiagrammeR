#' Get community membership using the Walktrap method
#' @description With the Walktrap community finding
#' algorithm, obtain the group membership values for
#' each of the nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param steps the number of steps to take for each
#' of the random walks.
#' @return a data frame with group membership
#' assignments for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the group membership values for
#' # all nodes in the graph through the
#' # Walktrap community finding algorithm
#' get_cmty_walktrap(graph)
#' #>    id walktrap_group
#' #> 1   1              1
#' #> 2   2              2
#' #> 3   3              2
#' #> 4   4              1
#' #> 5   5              1
#' #> 6   6              1
#' #> 7   7              2
#' #> 8   8              1
#' #> 9   9              2
#' #> 10 10              1
#'
#' # Add the group membership values to the
#' # graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_cmty_walktrap(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value walktrap_group
#' #> 1   1 <NA>     1   6.0              1
#' #> 2   2 <NA>     2   2.5              2
#' #> 3   3 <NA>     3   3.5              2
#' #> 4   4 <NA>     4   7.5              1
#' #> 5   5 <NA>     5   8.5              1
#' #> 6   6 <NA>     6   4.5              1
#' #> 7   7 <NA>     7  10.0              2
#' #> 8   8 <NA>     8  10.0              1
#' #> 9   9 <NA>     9   8.5              2
#' #> 10 10 <NA>    10  10.0              1
#' @importFrom igraph cluster_walktrap membership
#' @export get_cmty_walktrap

get_cmty_walktrap <- function(graph,
                              steps = 4) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_walktrap()` function
  cmty_walktrap_obj <-
    igraph::cluster_walktrap(ig_graph, steps = steps)

  # Create df with node memberships
  data.frame(
    id = igraph::membership(cmty_walktrap_obj) %>% names() %>% as.integer(),
    walktrap_group = as.vector(igraph::membership(cmty_walktrap_obj)),
    stringsAsFactors = FALSE)
}
