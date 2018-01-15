#' Is the graph an undirected graph?
#' @description Determines whether a graph
#' is set as undirected or not and
#' returns a logical value to that effect.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' # Create an empty graph; by
#' # default, new graphs made
#' # by `create_graph()` are
#' # directed graph, so, use
#' # `directed = FALSE` to create
#' # an undirected graph
#' graph <-
#'   create_graph(
#'     directed = FALSE)
#'
#' # Determine whether the
#' # graph is undirected
#' is_graph_undirected(graph)
#'
#' # Use the `set_graph_directed()`
#' # function and check again
#' # as to whether the graph is
#' # undirected
#' graph %>%
#'   set_graph_directed() %>%
#'   is_graph_undirected()
#' @export is_graph_undirected

is_graph_undirected <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Determine if graph is
  # undirected by using a
  # statement with `graph$directed`
  graph$directed == FALSE
}
