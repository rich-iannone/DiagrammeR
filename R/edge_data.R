#' Insert edge data attributes during edge creation
#' @description This helper function should be
#' invoked to provide values for the namesake
#' \code{edge_data} argument, which is present
#' in any function where edges are created.
#' @param ... edge data attributes supplied
#' as one or more named vectors.
#' @examples
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
#' #>   id from to  rel hour index
#' #> 1  1    1  2 <NA>    5     1
#' #> 2  2    2  3 <NA>    5     2
#' @export edge_data

edge_data <- function(...) {

  # Collect vectors of edge data
  # attribute values into a list object
  edge_data_values <- list(...)

  if (any(names(edge_data_values) %in% gv_edge_attributes())){

    stop("Names for edge data attributes shouldn't be any of those reserved for edge aesthetic attributes.")
  }

  edge_data_values
}
