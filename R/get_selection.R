#' Get the current selection available in a graph object
#' @description Get the current selection of nodes or edges from a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_node_df(create_nodes(1:6)) %>%
#'   add_edge("1", "2") %>% add_edge("1", "3") %>%
#'   add_edge("3", "4") %>% add_edge("4", "5") %>%
#'   add_edge("4", "6")
#'
#' # Select node "4", select all nodes a distance of 1 away from "4",
#' # and return the selection of nodes as a list object
#' graph %>% select_nodes(nodes = "4") %>%
#'   select_nodes_in_neighborhood(node = "4", distance = 1) %>%
#'   get_selection()
#' #> $nodes
#' #> [1] "4" "5" "6" "3"
#' }
#' @return a list object with the current selection of nodes or edges.
#' @export get_selection

get_selection <- function(graph){

  # If there is no selection available return NA value
  if (is.null(graph$selection)){
    return(NA)
  }

  selection <- graph$selection

  return(selection)
}
