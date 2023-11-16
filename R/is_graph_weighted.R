#' Is the graph a weighted graph?
#'
#' @description
#'
#' Provides a logical value on whether the graph is weighted. A graph is
#' considered to be weighted when it contains edges that all have a edge
#' `weight` attribute with numerical values assigned for all edges.
#'
#' @inheritParams render_graph
#'
#' @return A logical value.
#'
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
#'
#' @export
is_graph_weighted <- function(graph) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If the graph is empty or contains no edges, it cannot be
  # classified as a weighted graph
  if (nrow(graph$nodes_df) == 0 || nrow(graph$edges_df) == 0) {
    return(FALSE)
  }

  # Determine if the `weight` edge
  # attribute exists and contains non-NA
  # numerical values
  if ("weight" %in% colnames(graph$edges_df)) {

    if (inherits(graph$edges_df$weight, c("numeric", "integer")) &&
        !anyNA(graph$edges_df$weight)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}
