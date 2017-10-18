#' Insert node data attributes during node creation
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{node_data} argument, which is present
#' in any function where nodes are created.
#' @param ... node data attributes provided
#' as one or more named vectors.
#' @examples
#' \dontrun{
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
#' graph %>%
#'   get_node_df()
#' #>   id type label hour index
#' #> 1  1 path     1    5     1
#' #> 2  2 path     2    5     3
#' #> 3  3 path     3    5     2
#' }
#' @export node_data

node_data <- function(...) {

  # Collect vectors of node data
  # attribute values into a list object
  node_data_values <- list(...)

  if (any(names(node_data_values) %in% c(gv_node_attributes(), "x", "y"))){

    stop("Names for node data attributes shouldn't be any of those reserved for node aesthetic attributes.")
  }

  node_data_values
}
