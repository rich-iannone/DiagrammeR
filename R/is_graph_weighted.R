#' Is the graph a weighted graph?
#'
#' Provides a logical value on whether the graph is weighted. A graph is
#'   considered to be weighted when it contains edges that all have a edge
#'   `weight` attribute with numerical values assigned for all edges.
#' @inheritParams render_graph
#' @return a logical value.
#' @examples
#' # Create a graph where the edges have
#' # a `weight` attribute
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5) %>%
#'   select_edges() %>%
#'   set_edge_attrs_ws(
#'     edge_attr = weight,
#'     value = c(3, 5, 2, 9, 6)) %>%
#'   clear_selection()
#'
#' # Determine whether the graph
#' # is a weighted graph
#' graph %>% is_graph_weighted()
#'
#' # Create graph where the edges do
#' # not have a `weight` attribute
#' graph <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine whether this graph
#' # is weighted
#' graph %>% is_graph_weighted()
#' @export
is_graph_weighted <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If the graph is empty, it cannot be
  # classified as a weighted graph
  if (nrow(graph$nodes_df) == 0) {
    return(FALSE)
  }

  # If the graph contains no edges, it
  # cannot be a weighted graph
  if (nrow(graph$edges_df) == 0) {
    return(FALSE)
  }

  # Determine if the `weight` edge
  # attribute exists and contains non-NA
  # numerical values
  if ("weight" %in% colnames(graph$edges_df)) {

    if (inherits(graph$edges_df$weight, c("numeric", "integer")) &
        !any(is.na(graph$edges_df$weight))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
