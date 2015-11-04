#' Clear a selection of nodes or edges in a graph
#' @description Clear the selection of nodes or edges within a graph
#' object.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Select nodes "a" and "c"
#' graph <- select_nodes(graph = graph, nodes = c("a", "c"))
#'
#' # Verify that a node selection has been made
#' get_selection(graph)
#' #> $nodes
#' #> [1] "a" "c"
#'
#' # Clear the selection
#' graph <- clear_selection(graph = graph)
#'
#' # Verify that the node selection has been cleared
#' get_selection(graph)
#' #> [1] NA
#' }
#' @export clear_selection

clear_selection <- function(graph){

  # Clear the selection in the graph, if it exists
  if (!is.null(graph$selection)){
    graph$selection <- NULL
  }

  # Return the graph
  return(graph)
}
