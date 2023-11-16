#' Get radiality centrality scores
#'
#' @description
#'
#' Get the radiality centrality for all nodes in a graph. These scores describe
#' the ease to which nodes can reach other nodes.
#'
#' @inheritParams render_graph
#' @param direction Using `all` (the default), the search will ignore edge
#'   direction while traversing through the graph. With `out`, measurements of
#'   paths will be from a node whereas with `in`, measurements of paths will be
#'   to a node.
#'
#' @return A data frame with radiality centrality scores for each of the nodes.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get the radiality scores for nodes in the graph
#' graph %>%
#'   get_radiality()
#'
#' # Add the radiality values
#' # to the graph as a node
#' # attribute
#' graph <-
#'   graph %>%
#'   join_node_attrs(
#'     df = get_radiality(.))
#'
#' # Display the graph's node data frame
#' graph %>%
#'   get_node_df()
#'
#' @export
get_radiality <- function(
    graph,
    direction = "all"
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation Ensure that values provided for the
  # `direction` argument are from the
  # valid options
  rlang::arg_match0(direction, c("all", "in", "out"))

  # Get the number of nodes in the graph
  n_nodes <- count_nodes(graph)

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Get a matrix of shortest paths between all
  # pairs of nodes in the graph
  sp_matrix <-
    igraph::distances(
      graph = ig_graph,
      mode = direction,
      weights = NA)

  # Get the graph diameter
  is_directed <- direction != "all"

  diam <-
    igraph::diameter(
      graph = ig_graph,
      directed = is_directed)

  # Get the radiality values for all
  # nodes in the graph
  radiality_values <-
    apply(
      X = sp_matrix,
      MARGIN = 1,
      FUN = function(x) {
        if (all(is.infinite(x))) {
          return(0)
        } else {
          return(sum(diam + 1 - x[!is.infinite(x)])/(n_nodes - 1))
        }
      })

  # Create df with radiality scores
  data.frame(
    id = radiality_values %>%
      names() %>%
      as.integer(),
    radiality = radiality_values %>% round(4),
    stringsAsFactors = FALSE)
}
