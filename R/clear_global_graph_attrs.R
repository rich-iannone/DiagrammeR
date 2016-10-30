#' Clear any global graph attributes that are set v2
#' @description Clear any currently set global graph
#' attributes for a graph object of class
#' \code{dgr_graph}).
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and set some global attributes
#' graph <- create_graph() %>%
#'   set_global_graph_attrs_v2(
#'     "overlap", "true", "graph")
#'
#' # Clear all global attributes that have been set
#' graph <- clear_global_graph_attrs(graph)
#' @export clear_global_graph_attrs

clear_global_graph_attrs <- function(graph) {

  # Clear the global graph attributes data frame
  # by removing all rows from it
  graph$global_attrs <- graph$global_attrs[-(1:(nrow(graph$global_attrs))), ]

  return(graph)
}
