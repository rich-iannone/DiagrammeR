#' Get leverage centrality
#' @description Get the leverage centrality values
#' for all nodes in the graph. Leverage centrality
#' is a measure of the relationship between the degree
#' of a given node and the degree of each of its
#' neighbors, averaged over all neighbors. A node
#' with negative leverage centrality is influenced
#' by its neighbors, as the neighbors connect and
#' interact with far more nodes. A node with positive
#' leverage centrality influences its neighbors since
#' the neighbors tend to have far fewer connections.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame with leverage centrality
#' values for each of the nodes.
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
#' # Get leverage centrality values
#' # for all nodes in the graph
#' get_leverage_centrality(graph)
#' #>    id leverage_centrality
#' #> 1   1             -0.1964
#' #> 2   2             -0.1964
#' #> 3   3             -0.3810
#' #> 4   4              0.0556
#' #> 5   5             -0.0556
#' #> 6   6              0.0556
#' #> 7   7             -0.1964
#' #> 8   8             -0.3810
#' #> 9   9             -0.1964
#' #> 10 10             -1.0000
#'
#' # Add the leverage centrality
#' # values to the graph as a
#' # node attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_leverage_centrality(.))
#'
#' # Display the graph's node data frame
#' get_node_df(graph)
#' #>    id type label leverage_centrality
#' #> 1   1 <NA>  <NA>             -0.1964
#' #> 2   2 <NA>  <NA>             -0.1964
#' #> 3   3 <NA>  <NA>             -0.3810
#' #> 4   4 <NA>  <NA>              0.0556
#' #> 5   5 <NA>  <NA>             -0.0556
#' #> 6   6 <NA>  <NA>              0.0556
#' #> 7   7 <NA>  <NA>             -0.1964
#' #> 8   8 <NA>  <NA>             -0.3810
#' #> 9   9 <NA>  <NA>             -0.1964
#' #> 10 10 <NA>  <NA>             -1.0000
#' @importFrom igraph degree neighbors
#' @importFrom purrr map
#' @export get_leverage_centrality

get_leverage_centrality <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get degree values for all graph nodes
  degree_vals <- igraph::degree(ig_graph)

  # Calculate leverage centrality values for
  # all nodes in the graph
  leverage_centrality_values <-
    purrr::map(
      1:length(degree_vals),
      function(x) {
        mean(
          (degree_vals[x] - degree_vals[igraph::neighbors(ig_graph, degree_vals)]) /
            (degree_vals[x] + degree_vals[igraph::neighbors(ig_graph, degree_vals)]))
      }) %>%
    unlist()

  # Create df with leverage centrality values
  data.frame(
    id = degree_vals %>%
      names() %>%
      as.integer(),
    leverage_centrality = leverage_centrality_values %>% round(4),
    stringsAsFactors = FALSE)
}
