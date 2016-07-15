#' Get graph name
#' @description Get the name of a graph object of class
#' \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a name
#' graph <- set_graph_name(graph, "the_name")
#'
#' # Get the graph's name
#' get_graph_name(graph)
#' #> [1] "the_name"
#' @return a single-length character vector with the
#' assigned graph name. If a graph name has not been
#' set, NA is returned.
#' @export get_graph_name

get_graph_name <- function(graph) {

  if (is.null(graph$graph_name)) {
    graph_name <- NA
  }

  if (!is.null(graph$graph_name)) {
    graph_name <- graph$graph_name
  }

  return(graph_name)
}
