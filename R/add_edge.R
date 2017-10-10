#' Add an edge between nodes in a graph object
#' @description With a graph object of class
#' \code{dgr_graph}, add an edge to nodes within the
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param from the outgoing node from which the edge
#' is connected. There is the option to use a node
#' \code{label} value here (and this must
#' correspondingly also be done for the \code{to}
#' argument) for defining node connections. Note that
#' this is only possible if all nodes have distinct
#' \code{label} values set and none exist as an empty
#' string.
#' @param to the incoming nodes to which each edge
#' is connected. There is the option to use a node
#' \code{label} value here (and this must
#' correspondingly also be done for the \code{from}
#' argument) for defining node connections. Note that
#' this is only possible if all nodes have distinct
#' \code{label} values set and none exist as an empty
#' string.
#' @param rel an optional string specifying the
#' relationship between the
#' connected nodes.
#' @param ... optional edge attributes supplied as
#' vectors.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(label = "one") %>%
#'   add_node(label = "two") %>%
#'   add_node(label = "three") %>%
#'   add_node(label = "four")
#'
#' # Add an edge between those nodes and attach a
#' # relationship to the edge
#' graph <-
#'  add_edge(
#'    graph,
#'    from = 1,
#'    to = 2,
#'    rel = "A")
#'
#' # Use the `edge_info()` function to verify that
#' # the edge has been created
#' edge_info(graph)
#' #>   id from to rel
#' #> 1  1    1  2   A
#'
#' # Add another node and edge to the graph
#' graph <-
#'   graph %>%
#'   add_edge(
#'     from = 3,
#'     to = 2,
#'     rel = "A")
#'
#' # Verify that the edge has been created by
#' # getting a count of graph edges
#' count_edges(graph)
#' #> [1] 2
#'
#' # Add edges by specifying node `label`
#' # values; note that all nodes must have
#' # unique `label` values to use this option
#' graph <-
#'   graph %>%
#'   add_edge(
#'     from = "three",
#'     to = "four",
#'     rel = "L") %>%
#'   add_edge(
#'     from = "four",
#'     to = "one",
#'     rel = "L")
#'
#' # Use the `get_edges()` function to verify
#' # that the edges were added
#' get_edges(graph)
#' #> [1] "1->2" "3->2" "3->4" "4->1"
#' @importFrom dplyr bind_rows select filter
#' @importFrom tibble as_tibble
#' @importFrom rlang UQ
#' @export add_edge

add_edge <- function(graph,
                     from,
                     to,
                     rel = NULL,
                     ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, an edge cannot be added.")
  }

  if (length(from) > 1 | length(to) > 1) {
    stop("Only one edge can be specified.")
  }

  # Create bindings for specific variables
  version_id <- label <- NULL

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    graph$graph_log$version_id %>%
    max()

  if (is.null(rel)) {
    rel <- as.character(NA)
  }

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  # Collect extra vectors of data as `extras_tbl`
  if (length(extras) > 0) {
    extras_tbl <- tibble::as_tibble(extras)
  }

  # If `from` and `to` values provided as character
  # values, assume that these values refer to node
  # `label` attr values
  if (is.character(from) & is.character(to)) {

    # Stop function if the label for
    # `from` does not exist in the graph
    if (!(from %in% graph$nodes_df$label)) {
      stop("The value provided in `from` does not exist as a node `label` value.")
    }

    # Stop function if the label for
    # `from` is not distinct in the graph
    if (graph$nodes_df %>%
        dplyr::select(label) %>%
        dplyr::filter(label == from) %>%
        nrow() > 1) {
      stop("The node `label` provided in `from` is not distinct in the graph.")
    }

    # Stop function if the label for
    # `to` does not exist in the graph
    if (!(to %in% graph$nodes_df$label)) {
      stop("The value provided in `to` does not exist as a node `label` value.")
    }

    # Stop function if the label for
    # `to` is not distinct in the graph
    if (graph$nodes_df %>%
        dplyr::select(label) %>%
        dplyr::filter(label == to) %>%
        nrow() > 1) {
      stop("The node `label` provided in `to` is not distinct in the graph.")
    }

    # Use the `translate_to_node_id()` helper function to map
    # node `label` values to node `id` values
    from_to_node_id <-
      translate_to_node_id(
        graph = graph,
        from = from,
        to = to)

    from <- from_to_node_id$from
    to <- from_to_node_id$to
  }

  # Use `bind_rows()` to add an edge
  if (!is.null(graph$edges_df)) {

    combined_edges <-
      dplyr::bind_rows(
        graph$edges_df,
        data.frame(
          id = as.integer(graph$last_edge + 1),
          from = as.integer(from),
          to = as.integer(to),
          rel = as.character(rel),
          stringsAsFactors = FALSE))

    # Use the `combined_edges` object as a
    # replacement for the graph's internal
    # edge data frame
    graph$edges_df <- combined_edges

    if (exists("extras_tbl")) {

      # If extra edge attributes available, add
      # those to the new edge
      graph <-
        graph %>%
        select_edges_by_edge_id(
          edges = graph$edges_df$id %>% max())

      # Iteratively set edge attribute values for
      # the new edge in the graph
      for (i in 1:ncol(extras_tbl)) {
        graph <-
          graph %>%
          set_edge_attrs_ws(
            edge_attr = rlang::UQ(colnames(extras_tbl)[i]),
            value = extras_tbl[1, i][[1]])
      }

      # Clear the graph's active selection
      graph <-
        graph %>%
        clear_selection()
    }

    # Modify the `last_edge` vector
    graph$last_edge <- as.integer(graph$last_edge + 1)

    # Remove extra items from the `graph_log`
    graph$graph_log <-
      graph$graph_log %>%
      dplyr::filter(version_id <= current_graph_log_version_id)

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "add_edge",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

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
}
