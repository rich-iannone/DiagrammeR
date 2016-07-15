#' Add a multiple of new nodes with edges to or from
#' one or more selected nodes
#' @description Add n new nodes to or from one or more
#' nodes available as a selection in a graph object of
#' class \code{dgr_graph}. New graph edges will all
#' move either from the nodes in the selection toward
#' the newly created nodes (with the option
#' \code{direction = "from"}), or to the selected nodes
#' alredy in the graph (using \code{direction = "to"}).
#' Optionally, set node \code{type} and edge \code{rel}
#' values for all the new nodes and edges created,
#' respectively.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param n the number of new nodes to attach as
#' successor nodes to the nodes in the selection.
#' @param direction using \code{from} will create new
#' edges from existing nodes to the new nodes. The
#' \code{to} option will create new edges directed
#' toward the existing nodes.
#' @param set_node_type an optional string to apply a
#' \code{type} attribute to all newly created nodes.
#' @param set_edge_rel an optional string to apply a
#' \code{rel} attribute to all newly created edges.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create an empty graph, add a node to it, select
#' # that node, and then add 5 more nodes to the graph
#' # with edges from the original node to all of the
#' # new nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(1) %>%
#'   select_last_node %>%
#'   add_n_nodes_ws(5, "from")
#'
#' # Get the graph's nodes
#' graph %>% get_nodes
#' #> [1] "1" "2" "3" "4" "5" "6"
#'
#' # Get the graph's edges
#' graph %>% get_edges
#' #> "1 -> 2" "1 -> 3" "1 -> 4" "1 -> 5" "1 -> 6"
#'
#' # Create an empty graph, add a node to it, select
#' # that node, and then add 5 more nodes to the graph
#' # with edges toward the original node from all of
#' # the new nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(1) %>%
#'   select_last_node %>%
#'   add_n_nodes_ws(5, "to")
#'
#' # Get the graph's nodes
#' graph %>% get_nodes
#' #> [1] "1" "2" "3" "4" "5" "6"
#'
#' # Get the graph's edges
#' graph %>% get_edges
#' #> "2 -> 1" "3 -> 1" "4 -> 1" "5 -> 1" "6 -> 1"
#' @export add_n_nodes_ws

add_n_nodes_ws <- function(graph,
                           n,
                           direction = NULL,
                           set_node_type = NULL,
                           set_edge_rel = NULL) {

  # If no node selection is available, return
  # the graph unchanged
  if (is.null(graph$selection$nodes)) {
    return(graph)
  }

  # If the graph is directed and there is no value
  # given for the `direction` argument, stop function
  if (is_graph_directed(graph) &
      is.null(direction)) {
    stop("An edge direction must be provided.")
  }

  # If the graph is undirected, set the direction
  # to `to`
  if (is_graph_directed(graph) == FALSE) {
    direction <- "to"
  }

  # Get a vector of nodes available in the
  # graph's selection
  nodes_in_selection <- graph$selection$nodes

  # Case where nodes are added with edges from the
  # selected nodes
  if (direction == "from") {

    for (i in 1:length(nodes_in_selection)) {

      if (node_count(graph) == 0){
        node <- 1
      }

      if (node_count(graph) > 0){
        if (!is.na(
          suppressWarnings(
            any(as.numeric(get_nodes(graph)))))){

          numeric_components <-
            suppressWarnings(
              which(
                !is.na(as.numeric(get_nodes(graph)))))

          node <-
            max(
              as.integer(
                as.numeric(
                  get_nodes(graph)[
                    numeric_components]))) + 1
        }

        if (suppressWarnings(
          all(
            is.na(as.numeric(get_nodes(graph)))))){
          node <- 1
        }
      }

      if (!is.null(set_node_type)) {
        new_nodes <-
          create_nodes(
            nodes = seq(node, node + n - 1, 1),
            type = set_node_type)
      } else {
        new_nodes <-
          create_nodes(
            nodes = seq(node, node + n - 1, 1))
      }

      if (!is.null(set_edge_rel)) {
        new_edges <-
          create_edges(
            from = rep(nodes_in_selection[i], n),
            to = seq(node, node + n - 1, 1),
            rel = set_edge_rel)
      } else {
        new_edges <-
          create_edges(
            from = rep(nodes_in_selection[i], n),
            to = seq(node, node + n - 1, 1))
      }
    }
  }

  # Case where nodes are added with edges to the
  # selected nodes
  if (direction == "to") {

    for (i in 1:length(nodes_in_selection)) {

      if (node_count(graph) == 0){
        node <- 1
      }

      if (node_count(graph) > 0){
        if (!is.na(
          suppressWarnings(
            any(as.numeric(get_nodes(graph)))))){

          numeric_components <-
            suppressWarnings(
              which(
                !is.na(as.numeric(get_nodes(graph)))))

          node <-
            max(
              as.integer(
                as.numeric(
                  get_nodes(graph)[
                    numeric_components]))) + 1
        }

        if (suppressWarnings(
          all(
            is.na(as.numeric(get_nodes(graph)))))){
          node <- 1
        }
      }

      if (!is.null(set_node_type)) {
        new_nodes <-
          create_nodes(
            nodes = seq(node, node + n - 1, 1),
            type = set_node_type)
      } else {
        new_nodes <-
          create_nodes(
            nodes = seq(node, node + n - 1, 1))
      }

      if (!is.null(set_edge_rel)) {
        new_edges <-
          create_edges(
            from = seq(node, node + n - 1, 1),
            to = rep(nodes_in_selection[i], n),
            rel = set_edge_rel)
      } else {
        new_edges <-
          create_edges(
            from = seq(node, node + n - 1, 1),
            to = rep(nodes_in_selection[i], n))
      }
    }
  }

  # Add the new nodes to the graph
  graph <-
    add_node_df(graph, new_nodes)

  # Add the new edges to the graph
  graph <-
    add_edge_df(graph, new_edges)

  # Retain the currently selected nodes
  graph$selection$nodes <- nodes_in_selection

  return(graph)
}
