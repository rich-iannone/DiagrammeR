#' Traverse inward to a selected node, skipping over edges, and creating
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
#'   add_edge_df(create_edges(c("1", "1", "3", "4", "4"),
#'                            c("2", "3", "4", "5", "6")))
#'
#' # Starting from node "1", traverse nodes as far as possible then return
#' # back to node "1"
#' graph %>% select_nodes("1") %>%
#'   trav_out() %>% trav_out() %>% trav_out() %>%
#'   trav_in() %>% trav_in() %>% trav_in() %>%
#'   get_node_attr_from_selection()
#' #>   nodes type label
#' #> 1     1          1
#' }
#' @return a graph object of class \code{dgr_graph}.
#' @export trav_in

trav_in <- function(graph){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  if (is.null(graph$traversals)){
    return(graph)
  }

  current_selection <- graph$selection$nodes

  selection_node <- graph$traversals[[length(graph$traversals)]][1]


  # Determine whether traversals all contain the common starting node
  for (i in 1:length(graph$traversals)){
    if (i == 1) starting_node_present <- vector(mode = "logical")

    starting_node <- graph$traversals[[1]][1]

    if (graph$traversals[[i]][1] == starting_node){
      starting_node_present <- c(starting_node_present, TRUE)
    } else {
      starting_node_present <- c(starting_node_present, FALSE)
    }
  }

  if (all(starting_node_present)){

    graph$selection$nodes <- selection_node
    graph$traversals <- NULL

    return(graph)
  }

  if (length(graph$traversals) > 1){
    for (i in length(graph$traversals):1){
      if (graph$traversals[[i]][2] %in% selection_node){
        predecessor_to_selection_node <- graph$traversals[[i]][1]
      }
    }

    for (i in length(graph$traversals):1){
      if (i == length(graph$traversals)){
        all_selection_nodes <- vector(mode = "character")
      }

      if (graph$traversals[[i]][1] %in% predecessor_to_selection_node){
        all_selection_nodes <-
          c(all_selection_nodes,
            graph$traversals[[i]][2])
      }
    }

    graph$selection$nodes <- all_selection_nodes

    for (i in length(graph$traversals):1){
      if (graph$traversals[[i]][2] %in% current_selection){
        graph$traversals[[i]] <- NULL
      }
    }

    if (!is.null(graph$traversals)){

      if (length(graph$traversals) == 0){
        graph$traversals <- NULL
      }
    }

    return(graph)
  }
}
