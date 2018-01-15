#' Move layout positions of a selection of nodes
#' @description With an active selection of nodes,
#' move the position in either the \code{x} or
#' \code{y} directions, or both. Nodes in the
#' selection that do not have position information
#' (i.e., \code{NA} values for the \code{x} or
#' \code{y} node attributes) will be ignored.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param dx a single numeric value specifying the
#' amount that selected nodes (with non-\code{NA}
#' values for the \code{x} and \code{y} attributes)
#' will be moved in the x direction. A positive
#' value will move nodes right, negative left.
#' @param dy a single numeric value specifying the
#' amount that selected nodes (with non-\code{NA}
#' values for the \code{x} and \code{y} attributes)
#' will be moved in the y direction. A positive
#' value will move nodes up, negative down.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(
#'     type = "a",
#'     label = "one") %>%
#'   add_node(
#'     type = "a",
#'     label = "two") %>%
#'   add_node(
#'     type = "b",
#'     label = "three") %>%
#'   add_node(
#'     type = "b",
#'     label = "four")
#'
#' # Add position information to each of
#' # the graph's nodes
#' graph <-
#'   graph %>%
#'   set_node_position(
#'     node = 1, x = 1, y = 1) %>%
#'   set_node_position(
#'     node = 2, x = 2, y = 2) %>%
#'   set_node_position(
#'     node = 3, x = 3, y = 3) %>%
#'   set_node_position(
#'     node = 4, x = 4, y = 4)
#'
#' # Select all of the graph's nodes using the
#' # `select_nodes()` function (and only
#' # specifying the graph object)
#' graph <- select_nodes(graph)
#'
#' # Move the selected nodes (all the nodes,
#' # in this case) 5 units to the right
#' graph <-
#'   graph %>%
#'   nudge_node_positions_ws(
#'     dx = 5, dy = 0)
#'
#' # View the graph's node data frame
#' graph %>%
#'   get_node_df()
#'
#' # Now select nodes that have `type == "b"`
#' # and move them in the `y` direction 2 units
#' # (the graph still has an active selection
#' # and so it must be cleared first)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(
#'     conditions = type == "b") %>%
#'   nudge_node_positions_ws(
#'     dx = 0, dy = 2)
#'
#' # View the graph's node data frame
#' graph %>%
#'   get_node_df()
#' @importFrom dplyr filter case_when coalesce
#' @export nudge_node_positions_ws

nudge_node_positions_ws <- function(graph,
                                    dx,
                                    dy) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, no nodes can be moved.",
      call. = FALSE)
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    stop(
      "There is no selection of nodes available.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  nodes <- x <- y <- id <- NULL

  # Get the graph's node data frame as an object; stop
  # function if this doesn't exist
  if (is.null(graph$nodes_df)) {

    stop(
      "This graph does not contain any nodes.",
      call. = FALSE)

  } else {
    ndf <- graph$nodes_df
  }

  # If both the `x` and `y` attributes do not exist,
  # stop the function
  if (!("x" %in% colnames(ndf)) |
      !("y" %in% colnames(ndf))) {

    stop(
      "There are no `x` and `y` attribute values to modify.",
      call. = FALSE)
  }

  # Get the current selection of nodes
  nodes <- graph$node_selection$node

  # Determine which of the nodes selected have position
  # information set (i.e., not NA)
  ndf_filtered <-
    ndf %>%
    dplyr::filter(id %in% nodes) %>%
    dplyr::filter(!is.na(x) & !is.na(y))

  # If there are nodes to move, replace the `nodes`
  # vector with those node ID values; otherwise,
  # stop function
  if (nrow(ndf_filtered) == 0) {

    stop(
      "There are no nodes can be moved to different `x` or `y` locations.",
      call. = FALSE)

  } else {
    nodes <- ndf_filtered$id
  }

  # Use `case_when` statements to selectively perform
  # a vectorized `if` statement across all nodes for
  # the `x` and `y` node attribute
  ndf_new <-
    ndf %>%
    dplyr::mutate(x = dplyr::case_when(
      id %in% as.integer(nodes) ~ x + dx,
      !(id %in% as.integer(nodes)) ~ x)) %>%
    dplyr::mutate(y = dplyr::case_when(
      id %in% nodes ~ y + dy,
      !(id %in% as.integer(nodes)) ~ y))

  # Replace the graph's node data frame with `ndf_new`
  graph$nodes_df <- ndf_new

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "nudge_node_positions",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
