#' Insert edge data attributes during edge creation
#'
#' This helper function should be invoked to provide values for the namesake
#' \code{edge_data} argument, which is present in any function where edges are
#' created.
#' @param ... edge data attributes provided as one or more named vectors.
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
#' graph %>%
#'   get_edge_df()
#' }
#' @export
edge_data <- function(...) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Collect vectors of edge data
  # attribute values into a list object
  edge_data_values <- list(...)

  if (any(names(edge_data_values) %in% gv_edge_attributes())){

    emit_error(
      fcn_name = fcn_name,
      reasons = "Names for edge data attributes shouldn't be any of those reserved for edge aesthetic attributes")
  }

  edge_data_values
}
