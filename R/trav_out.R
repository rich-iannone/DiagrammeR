#' Traverse outward from a selected node, skipping over edges, and creating
#' a new node selection
#' @description From a graph object of class \code{dgr_graph} move outward
#' from one or more nodes present in a selection to other nodes, replacing
#' the current nodes in the selection with those nodes traversed to.
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
#'   add_edges(c("1", "1", "3", "4", "4"),
#'             c("2", "3", "4", "5", "6"))
#'
#' # Starting from node "1", traverse nodes twice then get the node
#' # attribute information at the landing nodes
#' graph %>% select_nodes("1") %>%
#'   trav_out() %>% trav_out() %>%
#'   get_node_attr_from_selection()
#' #>   nodes type label
#' #> 4     4          4
#' }
#' @return a graph object of class \code{dgr_graph}.
#' @export trav_out

trav_out <- function(graph){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  # Get all paths leading outward from node in selection
  distance_1_paths <-
    get_paths(graph = graph,
              from = graph$selection$nodes,
              distance = 1)

  # if NA returned, then there are no paths outward, so return
  # the same graph object without modifying the node selection
  if (length(distance_1_paths) == 1 & is.na(distance_1_paths)[1]){
    return(graph)
  }

  # For all valid paths returned, extract the nodes traversed to
  for (i in 1:length(distance_1_paths)){

    if (i == 1) landing_nodes <- vector(mode = "character")

    landing_nodes <-
      c(landing_nodes,
        distance_1_paths[[i]][length(distance_1_paths[[i]])])

    if (i == length(distance_1_paths)){
      landing_nodes <- unique(landing_nodes)
    }
  }

  # Update node selection in graph
  graph$selection$nodes <- landing_nodes

  graph$traversals <- c(graph$traversals, distance_1_paths)

  return(graph)
}
