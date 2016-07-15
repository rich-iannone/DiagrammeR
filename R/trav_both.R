#' Traverse from one or more selected nodes to
#' predecessors and successors, irrespective of edges,
#' creating a new node selection
#' @description From a graph object of class
#' \code{dgr_graph} move toward both predecessor and
#' successor nodes from one or more nodes present in a
#' selection. The current nodes in the selection are
#' replaced with those nodes traversed to.
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
#' library(magrittr)
#'
#' # Create a graph
#' graph <-
#' create_graph() %>%
#'   add_n_nodes(3) %>%
#'   add_edge(1, 2) %>%
#'   add_edge(2, 3)
#'
#' # Starting at node `2`, traverse to nodes `1` and
#' # `2` with `trav_both()` (where the traversal leads
#' # to any connected nodes, regardless of edge
#' # direction); store the traversed locations as a
#' # selection in the graph object
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both
#'
#' # Verify that the selection has been made by using
#' # the `get_selection()` function
#' get_selection(graph)
#' #> [1] "3" "1"
#'
#' # Modify the graph by adding `type` values for
#' # nodes `1` and `3`
#' graph <-
#'   graph %>%
#'   clear_selection %>%
#'   select_nodes_by_id(1) %>%
#'   set_node_attrs_ws(
#'     node_attr = "type", value = "a") %>%
#'   clear_selection %>%
#'   select_nodes_by_id(3) %>%
#'   set_node_attrs_ws(
#'     node_attr = "type", value = "z") %>%
#'   clear_selection
#'
#' # When traversing from node `2` to both `1` and `3`,
#' # you can set a condition that determines whether
#' # such traversal is permitted; in this case the
#' # condition is to traverse only to nodes where
#' # the type value is set to `a` (since node `3` has
#' # its `type` set to `z`, so, a traversal to `1`
#' # proceeds but there will be no traversal to `3`)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both("type", "a") %>%
#'   get_selection
#' #> [1] "1"
#'
#' # We can also set traversal conditions to satisfy
#' # numeric comparisons... the graph will be first
#' # modified
#' graph <-
#'   graph %>%
#'   set_node_attrs(1, "value", 3.4) %>%
#'   set_node_attrs(2, "value", 6.7) %>%
#'   set_node_attrs(3, "value", 9.1)
#' @export trav_both

trav_both <- function(graph,
                      node_attr = NULL,
                      match = NULL) {

  if (is.null(graph$selection$nodes)) {
    stop("There is no selection of nodes available.")
  }

  # Get the current selection of nodes
  selected_nodes <- get_selection(graph)

  # Get all paths leading outward from node in selection
  for (i in 1:length(selected_nodes)) {
    if (i == 1) {
      successors <- vector(mode = "character")
    }

    if (!is.na(
      get_successors(graph, selected_nodes[i])[1])) {
      successors <-
        c(successors,
          get_successors(graph = graph,
                         selected_nodes[i]))
    }

    if (i == length(selected_nodes)) {
      successors <- unique(successors)
    }
  }

  # Get all paths leading inward from node in selection
  for (i in 1:length(selected_nodes)) {
    if (i == 1) {
      predecessors <- vector(mode = "character")
    }

    if (!is.na(
      get_predecessors(graph,
                       selected_nodes[i])[1])) {
      predecessors <-
        c(predecessors,
          get_predecessors(graph = graph,
                           selected_nodes[i]))
    }

    if (i == length(selected_nodes)) {
      predecessors <- unique(predecessors)
    }
  }

  # If no successors and no predecessors returned then
  # there are no paths outward, so return the same
  # graph object without modifying the node selection
  if (length(successors) == 0 &
      length(predecessors) == 0) {
    return(graph)
  }

  if (length(successors) == 0) {
    succ_pred <- predecessors
  }

  if (length(predecessors) == 0) {
    succ_pred <- successors
  }

  if (length(successors) != 0 &
      length(predecessors) != 0) {
    succ_pred <- unique(c(successors, predecessors))
  }

  # If a `match` term provided, filter using a logical
  # expression or a regex match
  if (!is.null(match)) {

    if (grepl("^>.*", match) |
        grepl("^<.*", match) |
        grepl("^==.*", match) |
        grepl("^!=.*", match)) {
      logical_expression <- TRUE } else {
        logical_expression <- FALSE
      }

    if (logical_expression) {

      for (i in 1:length(succ_pred)) {

        if (i == 1) {
          to_nodes <- vector(mode = "character")
          column_number <-
            which(colnames(graph$nodes_df) %in%
                    node_attr)
        }

        if (grepl("^>.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    succ_pred[i]), column_number]) >
            as.numeric(gsub(">(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, succ_pred[i])
          }
        }

        if (grepl("^<.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    succ_pred[i]), column_number]) <
            as.numeric(gsub("<(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, succ_pred[i])
          }
        }

        if (grepl("^==.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    succ_pred[i]), column_number]) ==
            as.numeric(gsub("==(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, succ_pred[i])
          }
        }

        if (grepl("^!=.*", match)) {
          if (as.numeric(
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    succ_pred[i]), column_number]) !=
            as.numeric(gsub("!=(.*)", "\\1", match))) {

            to_nodes <- c(to_nodes, succ_pred[i])
          }
        }
      }
    }

    # Filter using a `match` value
    if (logical_expression == FALSE) {

      if (is.numeric(match)) {
        match <- as.character(match)
      }

      for (i in 1:length(succ_pred)) {

        if (i == 1) {
          to_nodes <- vector(mode = "character")

          column_number <-
            which(colnames(graph$nodes_df) %in%
                    node_attr)
        }

        if (match ==
            get_node_df(graph)[
              which(get_node_df(graph)[,1] %in%
                    succ_pred[i]),
              column_number]) {

          to_nodes <- c(to_nodes, succ_pred[i])
        }
      }
    }
    succ_pred <- to_nodes
  }

  # Update node selection in graph
  if (length(succ_pred) > 0) {
    graph$selection$nodes <- succ_pred
    return(graph)
  } else {
    return(graph)
  }
}
