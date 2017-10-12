#' Add a multiple of new nodes with edges to or from
#' one or more selected nodes
#' @description Add n new nodes to or from one or more
#' nodes available as a selection in a graph object of
#' class \code{dgr_graph}. New graph edges will all
#' move either from the nodes in the selection toward
#' the newly created nodes (with the option
#' \code{direction = "from"}), or to the selected nodes
#' already in the graph (using \code{direction = "to"}).
#' Optionally, set node \code{type} and edge \code{rel}
#' values for all the new nodes and edges created,
#' respectively.
#'
#' Selections of nodes can be performed using
#' the following \code{select_...} functions:
#' \code{select_nodes()},
#' \code{select_last_nodes_created()},
#' \code{select_nodes_by_degree()},
#' \code{select_nodes_by_id()}, or
#' \code{select_nodes_in_neighborhood()}.
#' Selections of nodes can also be performed using
#' the following traversal functions:
#' (\code{trav_...}):
#' \code{trav_out()}, \code{trav_in()},
#' \code{trav_both()}, \code{trav_in_node()},
#' \code{trav_out_node()}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of new nodes to attach as
#' successor nodes to the nodes in the selection.
#' @param direction using \code{from} will create new
#' edges from existing nodes to the new nodes. The
#' \code{to} option will create new edges directed
#' toward the existing nodes.
#' @param type an optional character vector that
#' provides group identifiers for the nodes to be added.
#' @param label an optional character object that
#' describes the nodes to be added.
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
#'   add_n_nodes(n = 1) %>%
#'   select_last_nodes_created() %>%
#'   add_n_nodes_ws(
#'     n = 5,
#'     direction = "from")
#'
#' # Get the graph's nodes
#' get_node_ids(graph)
#' #> [1] 1 2 3 4 5 6
#'
#' # Get the graph's edges
#' graph %>%
#'   get_edges()
#' #> "1->2" "1->3" "1->4" "1->5" "1->6"
#'
#' # Create an empty graph, add a node to it, select
#' # that node, and then add 5 more nodes to the graph
#' # with edges toward the original node from all of
#' # the new nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 1) %>%
#'   select_last_nodes_created() %>%
#'   add_n_nodes_ws(
#'     n = 5,
#'     direction = "to")
#'
#' # Get the graph's nodes
#' get_node_ids(graph)
#' #> [1] 1 2 3 4 5 6
#'
#' # Get the graph's edges
#' get_edges(graph)
#' #> "2->1" "3->1" "4->1" "5->1" "6->1"
#' @export add_n_nodes_ws

add_n_nodes_ws <- function(graph,
                           n,
                           direction = NULL,
                           type = NULL,
                           label = NULL,
                           rel = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes and existing nodes are required.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes, so, no new nodes can be added.")
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

  if (is.null(type)) {
    type <- as.character(NA)
  }

  if (is.null(label)) {
    label <- as.character(NA)
  }

  if (is.null(rel)) {
    rel <- as.character(NA)
  }

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Get a vector of nodes available in the
  # graph's selection
  nodes_in_selection <- graph$node_selection$node

  # Case where nodes are added with edges to the
  # selected nodes
  if (direction == "to") {

    for (i in 1:length(nodes_in_selection)) {

      graph <-
        add_n_nodes(
          graph = graph,
          n = n,
          type = type,
          label = label)

      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]

      for (j in (max(graph$nodes_df$id) - n + 1):max(graph$nodes_df$id)) {

        graph <-
        add_edge(
          graph = graph,
          from = j,
          to = nodes_in_selection[i],
          rel = rel)

        graph$graph_log <-
          graph$graph_log[-nrow(graph$graph_log), ]
      }
    }
  }

  # Case where nodes are added with edges from the
  # selected nodes
  if (direction == "from") {

    for (i in 1:length(nodes_in_selection)) {

      graph <-
        add_n_nodes(
          graph = graph,
          n = n,
          type = type,
          label = label)

      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]

      for (j in (max(graph$nodes_df$id) - n + 1):max(graph$nodes_df$id)) {

        graph <-
          add_edge(
            graph = graph,
            from = nodes_in_selection[i],
            to = j,
            rel = rel)

        graph$graph_log <-
          graph$graph_log[-nrow(graph$graph_log), ]
      }
    }
  }

  # Modify the graph object
  graph$directed <- ifelse(is_graph_directed(graph),
                           TRUE, FALSE)

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- graph %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_added <- nodes_graph_2 - nodes_graph_1

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_n_nodes_ws",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nodes_added,
      d_e = edges_added)

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
