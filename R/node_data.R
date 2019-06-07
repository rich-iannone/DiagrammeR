#' Insert node data attributes during node creation
#'
#' This helper function should be invoked to provide values for the namesake
#' `node_data` argument, which is present in any function where nodes are
#' created.
#'
#' @param ... Node data attributes provided as one or more named vectors.
#' @examples
#' # Create a new graph and add
#' # a path with several node
#' # data attributes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     type = "path",
#'     node_data = node_data(
#'       hour = 5,
#'       index = c(1, 3, 2)))
#'
#' # View the graph's internal
#' # node data frame; the node
#' # data attributes have been
#' # inserted
#' graph %>% get_node_df()
#'
#' @export
node_data <- function(...) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Collect vectors of node data
  # attribute values into a list object
  node_data_values <- list(...)

  if (any(names(node_data_values) %in% c(gv_node_attributes(), "x", "y"))){

    emit_error(
      fcn_name = fcn_name,
      reasons = "Names for node data attributes shouldn't be any of those reserved for node aesthetic attributes")
  }

  node_data_values
}
