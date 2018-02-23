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
#' @param edge_aes an optional list of named vectors
#' comprising edge aesthetic attributes. The helper
#' function \code{edge_aes()} is strongly recommended
#' for use here as it contains arguments for each
#' of the accepted edge aesthetic attributes (e.g.,
#' \code{shape}, \code{style}, \code{penwidth},
#' \code{color}).
#' @param edge_data an optional list of named vectors
#' comprising edge data attributes. The helper
#' function \code{edge_data()} is strongly recommended
#' for use here as it helps bind data specifically
#' to the created edges.
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
#' # Add an edge between those
#' # nodes and attach a
#' # relationship to the edge
#' graph <-
#'  add_edge(
#'    graph,
#'    from = 1,
#'    to = 2,
#'    rel = "A")
#'
#' # Use the `get_edge_info()`
#' # function to verify that
#' # the edge has been created
#' graph %>%
#'   get_edge_info()
#'
#' # Add another node and
#' # edge to the graph
#' graph <-
#'   graph %>%
#'   add_edge(
#'     from = 3,
#'     to = 2,
#'     rel = "A")
#'
#' # Verify that the edge
#' # has been created by
#' # counting graph edges
#' graph %>%
#'   count_edges()
#'
#' # Add edges by specifying
#' # node `label` values; note
#' # that all nodes must have
#' # unique `label` values to
#' # use this option
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
#' # Use `get_edges()` to verify
#' # that the edges were added
#' graph %>%
#'   get_edges()
#'
#' # Add edge aesthetic and data
#' # attributes during edge creation
#' graph_2 <-
#'   create_graph() %>%
#'   add_n_nodes(n = 2) %>%
#'   add_edge(
#'     from = 1,
#'     to = 2,
#'     rel = "M",
#'     edge_aes = edge_aes(
#'       penwidth = 1.5,
#'       color = "blue"),
#'     edge_data = edge_data(
#'       value = 4.3))
#'
#' # Use the `get_edges()` function
#' # to verify that the attribute
#' # values were bound to the
#' # newly created edge
#' graph_2 %>%
#'   get_edge_df()
#' @importFrom dplyr bind_rows select filter as_tibble
#' @importFrom rlang UQ
#' @export add_edge

add_edge <- function(graph,
                     from,
                     to,
                     rel = NULL,
                     edge_aes = NULL,
                     edge_data = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes, so, an edge cannot be added")
  }

  if (length(from) > 1 | length(to) > 1) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "Only one edge can be specified in `from` or `to`")
  }

  # Create bindings for specific variables
  version_id <- label <- index__ <- id <-  NULL

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    graph$graph_log$version_id %>%
    max()

  if (is.null(rel)) {
    rel <- as.character(NA)
  }

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- dplyr::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) == 1) {

      edge_aes$index__ <- 1

      edge_aes_tbl <-
        dplyr::as_tibble(edge_aes) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(edge_aes_tbl)) {
      edge_aes_tbl <-
        edge_aes_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect edge data attributes
  if (!is.null(edge_data)) {

    edge_data_tbl <- dplyr::as_tibble(edge_data)

    if (nrow(edge_data_tbl) == 1) {

      edge_data$index__ <- 1

      edge_data_tbl <-
        dplyr::as_tibble(edge_data) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(edge_data_tbl)) {
      edge_data_tbl <-
        edge_data_tbl %>%
        dplyr::select(-id)
    }
  }

  # If `from` and `to` values provided as character
  # values, assume that these values refer to node
  # `label` attr values
  if (is.character(from) & is.character(to)) {

    # Stop function if the label for
    # `from` does not exist in the graph
    if (!(from %in% graph$nodes_df$label)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The value provided in `from` does not exist as a node `label` value")
    }

    # Stop function if the label for
    # `from` is not distinct in the graph
    if (graph$nodes_df %>%
        dplyr::select(label) %>%
        dplyr::filter(label == from) %>%
        nrow() > 1) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The node `label` provided in `from` is not distinct in the graph")
    }

    # Stop function if the label for
    # `to` does not exist in the graph
    if (!(to %in% graph$nodes_df$label)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The value provided in `to` does not exist as a node `label` value")
    }

    # Stop function if the label for
    # `to` is not distinct in the graph
    if (graph$nodes_df %>%
        dplyr::select(label) %>%
        dplyr::filter(label == to) %>%
        nrow() > 1) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The node `label` provided in `to` is not distinct in the graph")
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

    if (exists("edge_aes_tbl")) {

      # If extra edge attributes available, add
      # those to the new edge
      graph <-
        suppressMessages(
          graph %>%
            select_edges_by_edge_id(
              edges = graph$edges_df$id %>% max())
        )

      # Iteratively set edge attribute values for
      # the new edge in the graph
      for (i in 1:ncol(edge_aes_tbl)) {
        graph <-
          graph %>%
          set_edge_attrs_ws(
            edge_attr = rlang::UQ(colnames(edge_aes_tbl)[i]),
            value = edge_aes_tbl[1, i][[1]])
      }

      # Clear the graph's active selection
      graph <-
        suppressMessages(
          graph %>%
            clear_selection())
    }

    if (exists("edge_data_tbl")) {

      # If extra edge attributes available, add
      # those to the new edge
      graph <-
        suppressMessages(
          graph %>%
            select_edges_by_edge_id(
              edges = graph$edges_df$id %>% max())
        )

      # Iteratively set edge attribute values for
      # the new edge in the graph
      for (i in 1:ncol(edge_data_tbl)) {
        graph <-
          graph %>%
          set_edge_attrs_ws(
            edge_attr = rlang::UQ(colnames(edge_data_tbl)[i]),
            value = edge_data_tbl[1, i][[1]])
      }

      # Clear the graph's active selection
      graph <-
        suppressMessages(
          graph %>%
            clear_selection())
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
        function_used = fcn_name,
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df),
        d_e = 1)

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
