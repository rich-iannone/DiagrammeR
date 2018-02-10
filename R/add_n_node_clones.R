#' Add one or several clones of an existing node to the graph
#' @description Add n new nodes to a graph object of
#' class \code{dgr_graph} which are clones of a node already
#' in the graph. All node attributes are preserved except for
#' the node \code{label} attribute (to maintain the
#' uniqueness of non-\code{NA} node label values). A vector
#' of node \code{label} can be provided to bind new labels
#' to the cloned nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of node clones to add to the graph.
#' @param node a node ID corresponding to the graph node
#' to be cloned.
#' @param label an optional vector of node label values.
#' The vector length should correspond to the value set
#' for \code{n}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a path of
#' # nodes; supply `label`, `type`,
#' # and `value` node attributes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     label = c("d", "g", "r"),
#'     type = c("a", "b", "c"))
#'
#' # Display the graph's internal
#' # node data frame
#' graph %>%
#'   get_node_df()
#'
#' # Create 3 clones of node `1`
#' # but assign new node label
#' # values (leaving `label` as
#' # NULL yields NA values)
#' graph <-
#'   graph %>%
#'   add_n_node_clones(
#'     n = 3,
#'     node = 1,
#'     label = c("x", "y", "z"))
#'
#' # Display the graph's internal
#' # node data frame: nodes `4`,
#' # `5`, and `6` are clones of `1`
#' graph %>%
#'   get_node_df()
#' @importFrom dplyr filter select
#' @export add_n_node_clones

add_n_node_clones <- function(graph,
                              n,
                              node,
                              label = NULL) {

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
      "The graph contains no nodes, so, an edge cannot be added.",
      call. = FALSE)
  }

  # Stop function if node is not a single numerical value
  if (length(node) > 1 | !inherits(node, "numeric")) {

    stop(
      "The value for `node` must be a single, numeric value.",
      call. = FALSE)
  }

  # Stop function the node ID does not correspond
  # to a node in the graph
  if (!(node %in% graph$nodes_df$id)) {

    stop(
      "The value provided in `node` does not correspond to a node in the graph.",
      call. = FALSE)
  }

  # Stop function if vector provided for label but it
  # is not of length `n`
  if (!is.null(label)) {
    if (length(label) != n) {

      stop(
        "The vector provided for `label` is not the same length as the value of `n`.",
        call. = FALSE)
    }
  }

  # Create bindings for specific variables
  id <- type <- version_id <- NULL

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    graph$graph_log$version_id %>%
    max()

  # Get the number of columns in the graph's
  # internal node data frame
  n_col_ndf <-
    graph %>%
    get_node_df() %>%
    ncol()

  # Extract all of the node attributes
  # (`type` and additional node attrs)
  if (n_col_ndf >= 4) {

    node_attr_vals <-
      graph %>%
      get_node_df() %>%
      dplyr::filter(id == node) %>%
      dplyr::select(type, 4:n_col_ndf)
  }

  # Create one or more clones of
  # the selected node in the graph
  graph <-
    graph %>%
    add_n_nodes(
      n = n,
      type = get_node_attrs(graph = graph, node_attr = type, nodes = node) %>% as.character(),
      label = label)

  # Obtain the node ID values for
  # the new nodes
  new_node_ids <-
    graph %>%
    select_last_nodes_created() %>%
    suppressMessages(get_selection())

  # Create a node selection for the
  # new nodes in the graph
  graph <-
    graph %>%
    select_nodes_by_id(
      nodes = new_node_ids)

  # Iteratively set node attribute values for
  # the new nodes in the graph

  if (exists("node_attr_vals")) {

    for (i in 1:ncol(node_attr_vals)) {
      for (j in 1:length(new_node_ids)) {

        graph$nodes_df[
          which(graph$nodes_df[, 1] == new_node_ids[j]),
          which(colnames(graph$nodes_df) == colnames(node_attr_vals)[i])] <-
          node_attr_vals[[i]]
      }
    }
  }

  # Clear the graph's active selection
  graph <-
    graph %>%
    suppressMessages(
      clear_selection())

  # Remove extra items from the `graph_log`
  graph$graph_log <-
    graph$graph_log %>%
    dplyr::filter(version_id <= current_graph_log_version_id)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_n_node_clones",
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
