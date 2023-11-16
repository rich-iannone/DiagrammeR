#' Is the graph empty?
#'
#' @description
#'
#' Provides a logical value on whether the graph is empty (i.e., contains no
#' nodes).
#'
#' @inheritParams render_graph
#'
#' @return A logical value.
#'
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Determine whether the graph is empty
#' graph %>% is_graph_empty()
#'
#' # Create a non-empty graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 3)
#'
#' # Determine whether this graph is empty
#' graph %>% is_graph_empty()
#'
#' @export
is_graph_empty <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Determine if graph is empty by checking the
  # number of rows in `graph$nodes_df`
  if (nrow(graph$nodes_df) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
