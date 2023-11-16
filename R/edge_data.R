#' Insert edge data attributes during edge creation
#'
#' @description
#'
#' This helper function should be invoked to provide values for the namesake
#' `edge_data` argument, which is present in any function where edges are
#' created.
#'
#' @param ... Edge data attributes provided as one or more named vectors.
#'
#' @examples
#' \dontrun{
#' # Create a new graph and add
#' # a path with several edge
#' # data attributes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     type = "path",
#'     edge_data = edge_data(
#'       hour = 5,
#'       index = c(1, 2)))
#'
#' # View the graph's internal
#' # edge data frame; the edge
#' # data attributes have
#' # been inserted
#' graph %>% get_edge_df()
#' }
#'
#' @family edge creation and removal
#'
#' @export
edge_data <- function(...) {

  # Collect vectors of edge data
  # attribute values into a list object
  edge_data_values <- list(...)

  if (any(names(edge_data_values) %in% gv_edge_attributes())) {

    cli::cli_abort(
      "Names for edge data attributes shouldn't be any of those reserved for edge aesthetic attributes.")
  }

  edge_data_values
}
