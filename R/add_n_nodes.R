#' Add one or several unconnected nodes to the graph
#'
#' Add \code{n} new nodes to a graph object of class \code{dgr_graph}.
#'   Optionally, set node \code{type} values for the new nodes.
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param n the number of new nodes to add to the graph.
#' @param type an optional character vector that provides group identifiers for
#'   the nodes to be added.
#' @param label an optional character object that describes the nodes to be
#'   added.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and
#' # add 5 nodes; these nodes
#' # will be assigned ID values
#' # from `1` to `5`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 5)
#'
#' # Get the graph's node IDs
#' graph %>% get_node_ids()
#' @importFrom dplyr select bind_cols bind_rows as_tibble
#' @export
add_n_nodes <- function(graph,
                        n,
                        type = NULL,
                        label = NULL,
                        node_aes = NULL,
                        node_data = NULL) {

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

  if (is.null(type)) {
    type <- as.character(NA)
  }

  if (is.null(label)) {
    label <- as.character(NA)
  }

  # Create bindings for specific variables
  id <- index__ <- NULL

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < n) {

      node_aes$index__ <- 1:n

      node_aes_tbl <-
        dplyr::as_tibble(node_aes) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(node_aes_tbl)) {
      node_aes_tbl <-
        node_aes_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect node data attributes
  if (!is.null(node_data)) {

    node_data_tbl <- dplyr::as_tibble(node_data)

    if (nrow(node_data_tbl) < n) {

      node_data$index__ <- 1:n

      node_data_tbl <-
        dplyr::as_tibble(node_data) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(node_data_tbl)) {
      node_data_tbl <-
        node_data_tbl %>%
        dplyr::select(-id)
    }
  }

  # Create a ndf of the correct length
  new_nodes <-
    create_node_df(
      n = n,
      type = type,
      label = label)

  # Add node aesthetics if available
  if (exists("node_aes_tbl")) {

    new_nodes <-
      new_nodes %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    new_nodes <-
      new_nodes %>%
      dplyr::bind_cols(node_data_tbl)
  }

  new_nodes[, 1] <- new_nodes[, 1] + graph$last_node

  graph$nodes_df <-
    dplyr::bind_rows(graph$nodes_df, new_nodes)

  # Update the `last_node` counter
  graph$last_node <- graph$last_node + n

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
      d_n = n)

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
