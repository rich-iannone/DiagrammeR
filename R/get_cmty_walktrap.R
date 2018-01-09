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
#' # Get the group membership
#' # values for all nodes in the
#' # graph through the Walktrap
#' # community finding algorithm
#' get_cmty_walktrap(graph)
#' #>    id walktrap_group
#' #> 1   1              3
#' #> 2   2              2
#' #> 3   3              2
#' #> 4   4              1
#' #> 5   5              2
#' #> 6   6              2
#' #> 7   7              3
#' #> 8   8              1
#' #> 9   9              1
#' #> 10 10              4
#'
#' # Add the group membership
#' # values to the graph as a
#' # node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_cmty_walktrap(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label walktrap_group
#' #> 1   1 <NA>  <NA>              3
#' #> 2   2 <NA>  <NA>              2
#' #> 3   3 <NA>  <NA>              2
#' #> 4   4 <NA>  <NA>              1
#' #> 5   5 <NA>  <NA>              2
#' #> 6   6 <NA>  <NA>              2
#' #> 7   7 <NA>  <NA>              3
#' #> 8   8 <NA>  <NA>              1
#' #> 9   9 <NA>  <NA>              1
#' #> 10 10 <NA>  <NA>              4
#' @importFrom igraph cluster_walktrap membership
#' @export get_cmty_walktrap

get_cmty_walktrap <- function(graph,
                              steps = 4) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
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
