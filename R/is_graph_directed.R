#' Is the graph a directed graph?
#'
#' @description
#'
#' Determines whether a graph is set to be directed or not and returns a logical
#' value to that effect.
#'
#' @inheritParams render_graph
#'
#' @return A logical value.
#'
#' @examples
#' # Create an empty graph; by default,
#' # new graphs made by `create_graph()`
#' # are directed
#' graph <- create_graph()
#'
#' # Determine whether the graph
#' # is directed
#' graph %>% is_graph_directed()
#'
#' # Use the `set_graph_undirected()`
#' # function and check again whether
#' # the graph is directed
#' graph %>%
#'   set_graph_undirected() %>%
#'   is_graph_directed()
#'
#' @export
is_graph_directed <- function(graph) {
  check_graph_valid(graph)
  # Determine if graph is directed by getting the
  # value at `graph$directed`
  graph$directed
}
