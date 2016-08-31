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
#' @param type an optional string to apply a
#' \code{type} attribute to all newly created nodes.
#' @param label a character object for supplying an
#' optional label to the node. Setting to \code{TRUE}
#' ascribes the node ID to the label. Setting to
#' \code{FALSE} yields a blank label.
#' @param rel an optional string to apply a
#' \code{rel} attribute to all newly created edges.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
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
#' #> [1] 1 2 3 4 5 6
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
#' #> [1] 1 2 3 4 5 6
#'
#' # Get the graph's edges
#' graph %>% get_edges
#' #> "2 -> 1" "3 -> 1" "4 -> 1" "5 -> 1" "6 -> 1"
#' @importFrom dplyr bind_rows
#' @export add_n_nodes_ws

add_n_nodes_ws <- function(graph,
                           n,
                           direction = NULL,
                           type = NULL,
                           label = TRUE,
                           rel = NULL) {

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

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get a vector of nodes available in the
  # graph's selection
  nodes_in_selection <- graph$selection$nodes

  # Case where nodes are added with edges from the
  # selected nodes
  if (direction == "from") {

    for (i in 1:length(nodes_in_selection)) {

      if (!is.null(type)) {
        new_nodes <-
          create_nodes(
            nodes = seq(nodes_created + 1,
                        nodes_created + n),
            type = type,
            label = label)
      } else {
        new_nodes <-
          create_nodes(
            nodes = seq(nodes_created + 1,
                        nodes_created + n),
            label = label)
      }

      if (!is.null(rel)) {
        new_edges <-
          create_edges(
            from = rep(nodes_in_selection[i], n),
            to = seq(nodes_created + 1,
                     nodes_created + n),
            rel = rel)
      } else {
        new_edges <-
          create_edges(
            from = rep(nodes_in_selection[i], n),
            to = seq(nodes_created + 1,
                     nodes_created + n))
      }

      new_edges[, 1] <- as.integer(new_edges[, 1])
      new_edges[, 2] <- as.integer(new_edges[, 2])

      nodes_created <- nodes_created + n
    }
  }

  # Case where nodes are added with edges to the
  # selected nodes
  if (direction == "to") {

    for (i in 1:length(nodes_in_selection)) {

      if (!is.null(type)) {
        new_nodes <-
          create_nodes(
            nodes = seq(nodes_created + 1,
                        nodes_created + n),
            type = type,
            label = label)
      } else {
        new_nodes <-
          create_nodes(
            nodes = seq(nodes_created + 1,
                        nodes_created + n),
            label = label)
      }

      if (!is.null(rel)) {
        new_edges <-
          create_edges(
            from = seq(nodes_created + 1,
                       nodes_created + n),
            to = rep(nodes_in_selection[i], n),
            rel = rel)
      } else {
        new_edges <-
          create_edges(
            from = seq(nodes_created + 1,
                       nodes_created + n),
            to = rep(nodes_in_selection[i], n))
      }

      nodes_created <- nodes_created + n
    }
  }

  # Add the new nodes to the graph
  combined_nodes <-
    dplyr::bind_rows(get_node_df(graph), new_nodes)

  if (!all(is.na(get_edge_df(graph)))) {
    combined_edges <-
      dplyr::bind_rows(get_edge_df(graph), new_edges)
  } else {
    combined_edges <- new_edges
  }

  # Create new graph object
  dgr_graph <-
    create_graph(
      nodes_df = combined_nodes,
      edges_df = combined_edges,
      graph_attrs = graph$graph_attrs,
      node_attrs = graph$node_attrs,
      edge_attrs = graph$edge_attrs,
      directed = ifelse(is_graph_directed(graph),
                        TRUE, FALSE),
      graph_name = graph$graph_name,
      graph_time = graph$graph_time,
      graph_tz = graph$graph_tz)

  # Retain the currently selected nodes
  dgr_graph$selection$nodes <- nodes_in_selection

  # Update the `last_node` counter
  dgr_graph$last_node <- nodes_created

  return(dgr_graph)
}
