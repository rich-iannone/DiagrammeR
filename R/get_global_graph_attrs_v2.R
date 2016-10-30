#' Get global graph attributes v2
#' @description Get the presently set global attributes
#' for a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @return a data frame containing global attributes
#' for the graph.
#' @examples
#' # Create a new graph and set some global attributes
#' graph <-
#'   create_graph() %>%
#'   set_global_graph_attrs_v2(
#'     "overlap", "true", "graph")
#'
#' # View the graph's set of global attributes
#' # as a data frame
#' get_global_graph_attrs_v2(graph)
#' #>      attr value attr_type
#' #> 1 overlap  true     graph
#' @export get_global_graph_attrs_v2

get_global_graph_attrs_v2 <- function(graph) {

  if (nrow(graph$global_attrs) == 0) {
    return(NA)
  } else if (nrow(graph$global_attrs) > 0) {
    return(graph$global_attrs)
  }
}
