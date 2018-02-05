#' Get global graph attributes
#' @description Get the available
#' global attributes for a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of
#' class \code{dgr_graph}.
#' @return a data frame containing
#' global attributes for the graph.
#' @examples
#' # Create a new, empty graph
#' graph <- create_graph()
#'
#' # View the graph's set of
#' # global attributes
#' graph %>%
#'   get_global_graph_attr_info()
#' @importFrom dplyr tibble as_tibble
#' @export get_global_graph_attr_info

get_global_graph_attr_info <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
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
