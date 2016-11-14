#' Set graph name
#' @description Set a name for a graph object of class
#' \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param name the name to set for the graph.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Provide the new graph with a name
#' graph <- set_graph_name(graph, "example_name")
#' @return a graph object of class \code{dgr_graph}.
#' @export set_graph_name

set_graph_name <- function(graph,
                           name) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Set the graph's name
  graph$graph_info$graph_name[1] <-
    as.character(name)

  return(graph)
}
