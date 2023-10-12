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

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Determine if graph is empty by checking the
  # number of rows in `graph$nodes_df`
  if (nrow(graph$nodes_df) == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
