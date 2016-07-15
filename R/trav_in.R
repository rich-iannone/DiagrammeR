#' Traverse inward to a selected node, skipping over
#' edges, and creating a new node selection
#' @description From a graph object of class
#' \code{dgr_graph} move outward from one or more nodes
#' present in a selection to other nodes, replacing
#' the current nodes in the selection with those nodes
#' traversed to.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr an optional character vector of
#' node attribute values for filtering the node ID
#' values returned.
#' @param match an option to provide a logical
#' expression with a comparison operator (\code{>},
#' \code{<}, \code{==}, or \code{!=}) followed by a
#' number for numerical filtering, or, a character
#' string for filtering the edges returned through
#' string matching.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a graph
#' graph <-
#' create_graph() %>%
#'   add_n_nodes(4) %>%
#'   add_edge(1, 2) %>%
#'   add_edge(2, 3) %>%
#'   add_edge(3, 4)
#'
#' # Starting at node `4`, traverse to node `1`,
#' # storing the traversed location as a selection in
#' # the graph object
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(4) %>%
#'   trav_in %>% trav_in %>% trav_in
#'
#' # Verify that the selection has been made by using
#' # the `get_selection()` function
#' get_selection(graph)
#' #> [1] "1"
#'
#' # Modify the graph by adding `type` values for
#' # each of the nodes
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(4) %>%
#'   set_node_attrs_ws(
#'     node_attr = "type", value = "z") %>%
#'   clear_selection %>%
#'   select_nodes_by_id(1:3) %>%
#'   set_node_attrs_ws(
#'     node_attr = "type", value = "a") %>%
#'   clear_selection
#'
#' # When traversing inward from node `4` to `3`,
#' # you can set a condition that determines whether
#' # such traversal is permitted; in this case the
#' # condition is to traverse only to nodes where
#' # the type value is set to `z` (but node `3` has
#' # its `type` set to `a`, so, no traversal)
#' graph %>%
#'   select_nodes_by_id(4) %>%
#'   trav_in("type", "z") %>%
#'   get_selection
#' #> [1] "4"
#'
#' # Setting the condition to traverse only to nodes
#' # where the type is `a` will result in a traversal
#' graph %>%
#'   select_nodes_by_id(4) %>%
#'   trav_in("type", "a") %>%
#'   get_selection
#' #> [1] "3"
#'
#' # We can also set traversal conditions to satisfy
#' # numeric comparisons... the graph will be first
#' # modified
#' graph <-
#'   graph %>%
#'   set_node_attrs(1, "value", 3.4) %>%
#'   set_node_attrs(2, "value", 6.7) %>%
#'   set_node_attrs(3, "value", 9.1) %>%
#'   set_node_attrs(4, "value", 5.5)
#'
#' # Traverse from nodes `4` to `1`, setting the
#' # condition that each node traversed to must have
#' # a `value` greater than 5.0 (although 3 separate
#' # traversals are intended, the last traversal does
#' # not reach node `1` since its value is not
#' # greater than 5.0)
#' graph %>%
#'   select_nodes_by_id(4) %>%
#'   trav_in("value", ">5.0") %>%
#'   trav_in("value", ">5.0") %>%
#'   trav_in("value", ">5.0") %>%
#'   get_selection
#' #> [1] "2"
#' }
#' @export trav_in

trav_in <- function(graph,
                    node_attr = NULL,
                    match = NULL) {

  if (is.null(graph$selection$nodes)) {
    stop("There is no selection of nodes available.")
  }

  # Get the current selection of nodes
  selected_nodes <- get_selection(graph)

  # Get all paths leading inward from node in selection
  for (i in 1:length(selected_nodes)) {
    if (i == 1) predecessors <- vector(mode = "character")

    if (!is.na(
      get_predecessors(graph, selected_nodes[i])[1])) {
      predecessors <-
        c(predecessors,
          get_predecessors(
            graph = graph, selected_nodes[i]))
    }

    if (i == length(selected_nodes)) {
      predecessors <- unique(predecessors)
    }
  }

  # if no predecessors returned, then there are no
  # paths outward, so return the same graph object
  # without modifying the node selection
  if (length(predecessors) == 0) {
    return(graph)
  }

  # If a match term provided, filter using a logical
  # expression or a regex match
  if (!is.null(match)) {

    if (grepl("^>.*", match) | grepl("^<.*", match) |
        grepl("^==.*", match) | grepl("^!=.*", match)) {
      logical_expression <- TRUE } else {
        logical_expression <- FALSE
      }

    if (logical_expression) {

      for (i in 1:length(predecessors)) {

        if (i == 1) {
          to_nodes <- vector(mode = "character")

          column_number <-
            which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (grepl("^>.*", match)) {
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  predecessors[i]), column_number]) >
              as.numeric(gsub(">(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }

        if (grepl("^<.*", match)) {
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  predecessors[i]), column_number]) <
              as.numeric(gsub("<(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }

        if (grepl("^==.*", match)) {
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  predecessors[i]), column_number]) ==
              as.numeric(gsub("==(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }

        if (grepl("^!=.*", match)) {
          if (as.numeric(get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                                  predecessors[i]), column_number]) !=
              as.numeric(gsub("!=(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, predecessors[i])
          }
        }
      }
    }

    # Filter using a `match` value
    if (logical_expression == FALSE) {

      if (is.numeric(match)) {
        match <- as.character(match)
      }

      for (i in 1:length(predecessors)) {
        if (i == 1) {
          to_nodes <- vector(mode = "character")
          column_number <- which(colnames(graph$nodes_df) %in% node_attr)
        }

        if (match ==
            get_node_df(graph)[which(get_node_df(graph)[,1] %in%
                                     predecessors[i]), column_number]) {

          to_nodes <- c(to_nodes, predecessors[i])
        }
      }
    }
    predecessors <- to_nodes
  }

  # Update node selection in graph
  if (length(predecessors) > 0) {
    graph$selection$nodes <- predecessors
    return(graph)
  } else {
    return(graph)
  }
}
