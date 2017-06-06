#' Is the graph empty?
#' @description Provides a logical value on whether the
#' graph is empty (i.e., contains no nodes).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a logical value.
#' @examples
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
#'   add_n_nodes(n = 3)
#'
#' # Determine whether this graph is empty
#' is_graph_empty(graph)
#' #> [1] FALSE
#' @export is_graph_empty

is_graph_empty <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Determine if graph is empty by checking the
  # number of rows in `graph$nodes_df`
  if (nrow(graph$nodes_df) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
