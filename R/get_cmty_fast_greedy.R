#' Get community membership by modularity optimization
#' @description Through the use of greedy optimization
#' of a modularity score, obtain the group membership
#' values for each of the nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with group membership
#' assignments for each of the nodes.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the group membership values for all
#' # nodes in the graph through the greedy
#' # optimization of modularity algorithm
#' get_cmty_fast_greedy(graph)
#' #>    id f_g_group
#' #> 1   1         1
#' #> 2   2         2
#' #> 3   3         2
#' #> 4   4         1
#' #> 5   5         1
#' #> 6   6         1
#' #> 7   7         2
#' #> 8   8         1
#' #> 9   9         2
#' #> 10 10         1
#'
#' # Add the group membership values to the
#' # graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_cmty_fast_greedy(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label value f_g_group
#' #> 1   1 <NA>     1   6.0         1
#' #> 2   2 <NA>     2   2.5         2
#' #> 3   3 <NA>     3   3.5         2
#' #> 4   4 <NA>     4   7.5         1
#' #> 5   5 <NA>     5   8.5         1
#' #> 6   6 <NA>     6   4.5         1
#' #> 7   7 <NA>     7  10.0         2
#' #> 8   8 <NA>     8  10.0         1
#' #> 9   9 <NA>     9   8.5         2
#' #> 10 10 <NA>    10  10.0         1
#' @importFrom igraph cluster_fast_greedy membership
#' @export get_cmty_fast_greedy

get_cmty_fast_greedy <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If graph is directed, transform to undirected
  graph <- set_graph_undirected(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the community object using the
  # `cluster_fast_greedy()` function
  cmty_fast_greedy_obj <-
    igraph::cluster_fast_greedy(ig_graph)

  # Create df with node memberships
  data.frame(
    id = as.integer(names(igraph::membership(cmty_fast_greedy_obj))),
    f_g_group = as.vector(igraph::membership(cmty_fast_greedy_obj)),
    stringsAsFactors = FALSE)
}
