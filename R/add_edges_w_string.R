#' Add one or more edges using a text string
#'
#' @description
#'
#' With a graph object of class `dgr_graph`, add one or more edges to the graph
#' using a text string.
#'
#' @inheritParams render_graph
#' @param edges A single-length vector with a character string specifying the
#'   edges. For a directed graph, the string object should be formatted as a
#'   series of node ID values as `[node_ID_1]->[node_ID_2]` separated by a one
#'   or more space characters. For undirected graphs, `--` should replace `->`.
#'   Line breaks in the vector won't cause an error.
#' @param rel An optional vector specifying the relationship between the
#'   connected nodes.
#' @param use_labels An option to use node `label` values in the `edges` string
#'   to define node connections. Note that this is only possible if all nodes
#'   have distinct `label` values set and none exist as an empty string.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(label = "one") %>%
#'   add_node(label = "two") %>%
#'   add_node(label = "three") %>%
#'   add_node(label = "four")
#'
#' # Add edges between nodes using
#' # a character string with node
#' # ID values
#' graph_node_id <-
#'   graph %>%
#'   add_edges_w_string(
#'     edges = "1->2 1->3 2->4 2->3")
#'
#' # Show the graph's internal
#' # edge data frame
#' graph_node_id %>% get_edge_df()
#'
#' # Add edges between nodes using
#' # a character string with node
#' # label values and setting
#' # `use_labels = TRUE`; note that
#' # all nodes must have unique
#' # `label` values to use this
#' graph_node_label <-
#'   graph %>%
#'   add_edges_w_string(
#'     edges =
#'       "one->two one->three
#'        two->four two->three",
#'     use_labels = TRUE)
#'
#' # Show the graph's internal
#' # edge data frame (it's the
#' # same as before)
#' graph_node_label %>% get_edge_df()
#'
#' @family edge creation and removal
#'
#' @export
add_edges_w_string <- function(
    graph,
    edges,
    rel = NULL,
    use_labels = FALSE
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    max(graph$graph_log$version_id)

  # Remove linebreak characters from `edges`
  edges_cleaned <-
    gsub("\n", " ", edges)

  # Remove extra spaces within the string
  edges_cleaned <-
    gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
         edges_cleaned, perl = TRUE)

  # Split by single spaces into separate edge
  # expressions
  edges_split <-
    unlist(strsplit(edges_cleaned, " "))

  # Split the edge expressions in a directed
  # graph into `from` and `to` vectors
  if (graph$directed) {
    from <-
      sapply(strsplit(edges_split, "->"), "[[", 1)

    to <-
      sapply(strsplit(edges_split, "->"), "[[", 2)
  }

  # Split the edge expressions in an undirected
  # graph into `from` and `to` vectors
  if (!graph$directed) {
    from <-
      sapply(strsplit(edges_split, "--"), "[[", 1)

    to <-
      sapply(strsplit(edges_split, "--"), "[[", 2)
  }

  # If `use_label` is set to TRUE, treat values in
  # list as labels; need to map to node ID values
  if (use_labels) {
    from_to_node_id <-
      translate_to_node_id(
        graph = graph,
        from = from,
        to = to)

    from <- from_to_node_id$from
    to <- from_to_node_id$to
  }

  # Create an edge data frame (edf) without
  # associated `rel` values
  if (is.null(rel)) {
    new_edges <-
      create_edge_df(
        from = from,
        to = to)
  }

  # Create an edge data frame (edf) with
  # associated `rel` values
  if (!is.null(rel)) {
    new_edges <-
      create_edge_df(
        from = from,
        to = to,
        rel = rel)
  }

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Add the new edges to the graph
  graph <- add_edge_df(graph, new_edges)

  # Get the updated number of edges in the graph
  edges_graph_2 <- graph %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

  # Clear the graph's active selection
  graph <-
    suppressMessages(
      graph %>%
        clear_selection())

  # Remove extra items from the `graph_log`
  graph$graph_log <-
    graph$graph_log %>%
    dplyr::filter(version_id <= current_graph_log_version_id)

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_e = edges_added)

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      trigger_graph_actions(graph)
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
