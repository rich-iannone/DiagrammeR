#' Get global graph attributes
#'
#' @description
#'
#' Get the available global attributes for a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#'
#' @return A data frame containing global attributes for the graph.
#'
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
      reasons = "The graph object is not valid"
    )
  }

  if (nrow(graph$global_attrs) == 0) {

    global_graph_attrs_tbl <-
      dplyr::tibble(
        attr = character(0),
        value = character(0),
        attr_type = character(0)
      )

  } else if (nrow(graph$global_attrs) > 0) {
    global_graph_attrs_tbl <- dplyr::as_tibble(graph$global_attrs)
  }

  global_graph_attrs_tbl
}
