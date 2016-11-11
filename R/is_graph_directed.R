#' Is the graph a directed graph?
#' @description Determines whether a graph is set to be
#' directed or not and returns a logical value to that
#' effect.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' # Create an empty graph; by default, new graphs
#' # made by `create_graph()` are directed`
#' graph <-
#'   create_graph()
#'
#' # Determine whether the graph is directed
#' is_graph_directed(graph)
#' #> [1] TRUE
#'
#' # Use the `set_graph_undirected()` function and
#' # check again whether the graph is directed
#' graph %>%
#'   set_graph_undirected() %>%
#'   is_graph_directed()
#' #> [1] FALSE
#' @export is_graph_directed

is_graph_directed <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Determine if graph is directed by getting the
  # value at `graph$directed`
  graph_is_directed <- graph$directed

  return(graph_is_directed)
}
