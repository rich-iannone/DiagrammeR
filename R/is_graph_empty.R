#' Is the graph empty?
#' @description Provides a logical value on whether the
#' graph is empty (i.e., contains no nodes).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
#' library(magrittr)
#'
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Determine whether the graph is empty
#' is_graph_empty(graph)
#' #> [1] TRUE
#'
#' # Create a non-empty graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(3)
#'
#' # Determine whether this graph is empty
#' is_graph_empty(graph)
#' #> [1] FALSE
#' @export is_graph_empty

is_graph_empty <- function(graph) {

  # Determine if graph is empty by checking for
  # a NULL value at `graph$nodes_df`
  graph_is_empty <- is.null(graph$nodes_df)

  return(graph_is_empty)
}
