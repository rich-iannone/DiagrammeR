#' Get the current selection available in a graph
#' object
#' @description Get the current selection of nodes or
#' edges from a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_node_df(create_nodes(1:6)) %>%
#'   add_edge("1", "2") %>%
#'   add_edge("1", "3") %>%
#'   add_edge("3", "4") %>%
#'   add_edge("4", "5") %>%
#'   add_edge("4", "6")
#'
#' # Select node `4`, then select all nodes a
#' # distance of 1 away from node `4`, and finally
#' # return the selection of nodes as a vector object
#' graph %>%
#'   select_nodes(nodes = "4") %>%
#'   select_nodes_in_neighborhood(
#'     node = "4", distance = 1) %>%
#'   get_selection
#' #> [1] "4" "5" "6" "3"
#'
#' # Select edges associated with node `4` and return
#' # the selection of edges
#' graph %>%
#'   select_edges_by_node_id(4) %>%
#'   get_selection
#' #> [1] "4 -> 5" "4 -> 6" "3 -> 4"
#' @return a vector with the current selection of nodes
#' or edges.
#' @export get_selection

get_selection <- function(graph) {

  # If there is no selection available, return NA
  if (is.null(graph$selection)) {
    return(NA)
  }

  if (names(graph$selection) == 'nodes') {
    selection <- graph$selection[[1]]
    return(selection)
  }

  if (names(graph$selection) == 'edges') {
    selection_from <- graph$selection[[1]][[1]]
    selection_to <- graph$selection[[1]][[2]]

    if (is_graph_directed(graph)) {
      selection <-
        paste(selection_from, selection_to, sep = " -> ")
    }

    if (is_graph_directed(graph) == FALSE) {
      selection <-
        paste(selection_from, selection_to, sep = " - ")
    }

    return(selection)
  }
}
