#' Get closeness vitality
#' @description Get the closeness vitality values
#' for all nodes in the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with closeness vitality
#' values for each of the nodes.
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
#' # Get closeness vitality values for
#' # all nodes in the graph
#' get_closeness_vitality(graph)
#' #>    id closeness_vitality
#' #> 1   1                 32
#' #> 2   2                118
#' #> 3   3                 36
#' #> 4   4                 60
#' #> 5   5                  0
#' #> 6   6                 48
#' #> 7   7                 46
#' #> 8   8                 30
#' #> 9   9                 48
#' #> 10 10                 44
#'
#' # Add the closeness vitality values
#' # to the graph as a node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_closeness_vitality(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label closeness_vitality
#' #> 1   1 <NA>  <NA>                 32
#' #> 2   2 <NA>  <NA>                118
#' #> 3   3 <NA>  <NA>                 36
#' #> 4   4 <NA>  <NA>                 60
#' #> 5   5 <NA>  <NA>                  0
#' #> 6   6 <NA>  <NA>                 48
#' #> 7   7 <NA>  <NA>                 46
#' #> 8   8 <NA>  <NA>                 30
#' #> 9   9 <NA>  <NA>                 48
#' #> 10 10 <NA>  <NA>                 44
#' @importFrom igraph distances delete_vertices
#' @importFrom purrr map
#' @export get_closeness_vitality

get_closeness_vitality <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get the sum of distances between all pairs
  # of nodes in the graph
  sum_distances <- sum(igraph::distances(ig_graph))

  # Calculated closeness vitality values for
  # all nodes in the graph
  closeness_vitality_values <-
    purrr::map(
      1:nrow(graph$nodes_df),
      function(x) {
        distances <- igraph::distances(igraph::delete_vertices(ig_graph, x))
        sum_distances - sum(distances[!is.infinite(distances)])
      }) %>% unlist()

  # Create df with closeness vitality values
  data.frame(
    id = graph$nodes_df$id,
    closeness_vitality = closeness_vitality_values,
    stringsAsFactors = FALSE)
}
