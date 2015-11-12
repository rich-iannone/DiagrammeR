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

  # Get the current selection of nodes
  selected_nodes <- get_selection(graph)$nodes

  # Get all paths leading inward from node in selection
  for (i in 1:length(selected_nodes)){
    if (i == 1) predecessors <- vector(mode = "character")

    if (!is.na(get_predecessors(graph, selected_nodes[i])[1])){
      predecessors <-
        c(predecessors,
          get_predecessors(graph = graph, selected_nodes[i]))
    }

    if (i == length(selected_nodes)){
      predecessors <- unique(predecessors)
    }
  }

  # if no predecessors returned, then there are no paths outward,
  # so return the same graph object without modifying the node selection
  if (length(predecessors) == 0){
    return(graph)
  }

  # If a search term provided, filter using a logical expression
  # or a regex match
  if (!is.null(search)){

    if (grepl("^>.*", search) | grepl("^<.*", search) |
        grepl("^==.*", search) | grepl("^!=.*", search)){
      logical_expression <- TRUE } else {
        logical_expression <- FALSE
      }

    if (logical_expression){

      for (i in 1:length(predecessors)){

        if (i == 1){
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (grepl("^>.*", search)){
          if (as.numeric(get_node_attr(graph,
                                       nodes = predecessors[i])[1,column_number]) >
              as.numeric(gsub(">(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }

        if (grepl("^<.*", search)){
          if (as.numeric(get_node_attr(graph,
                                       nodes = predecessors[i])[1,column_number]) <
              as.numeric(gsub("<(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }

        if (grepl("^==.*", search)){
          if (as.numeric(get_node_attr(graph,
                                       nodes = predecessors[i])[1,column_number]) ==
              as.numeric(gsub("==(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }

        if (grepl("^!=.*", search)){
          if (as.numeric(get_node_attr(graph,
                                       nodes = predecessors[i])[1,column_number]) !=
              as.numeric(gsub("!=(.*)", "\\1", search))){

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }
      }
    }

    # Filter using a `search` value as a regular expression
    if (logical_expression == FALSE){

      for (i in 1:length(predecessors)){

        if (i == 1){
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (grepl(search, get_node_attr(graph,
                                        nodes = predecessors[i])[1,column_number])){

          to_nodes <- c(to_nodes, predecessors[i])
        }
      }
    }

    predecessors <- to_nodes
  }

  # Update node selection in graph
  graph$selection$nodes <- predecessors

  return(graph)
}
