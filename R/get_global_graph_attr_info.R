#' Get global graph attributes
#'
#' Get the available global attributes for a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#' @return A data frame containing global attributes for the graph.
#' @examples
#' # Create a new, empty graph
#' graph <- create_graph()
#'
#' # View the graph's set of
#' # global attributes
#' graph %>%
#'   get_global_graph_attr_info()
#'
#' @export
get_global_graph_attr_info <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  if (nrow(graph$global_attrs) == 0) {

    global_graph_attrs_tbl <-
      dplyr::tibble(
        attr = NA_character_ ,
        value = NA_character_,
        attr_type = NA_character_)[-1, ]

  } else if (nrow(graph$global_attrs) > 0) {

    global_graph_attrs_tbl <-
      graph$global_attrs %>%
      dplyr::as_tibble()
  }

  global_graph_attrs_tbl
}
