#' Get information on any available graph actions
#' @description Get a tibble of the available graph
#' actions, which contains information on function
#' invocations to be called on the graph at every
#' transformation step, or, when manually invoked
#' with the \code{trigger_graph_actions()} function.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a \code{df_tbl} object.
#' @importFrom tibble as_tibble
#' @export get_graph_actions

get_graph_actions <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  graph$graph_actions %>% tibble::as_tibble()
}
