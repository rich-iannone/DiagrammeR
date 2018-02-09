#' Select nodes in a graph
#' @description Select nodes from a graph object of
#' class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param conditions an option to use filtering
#' conditions for the retrieval of nodes.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @param nodes an optional vector of node IDs for
#' filtering the list of nodes present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "z", "z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("a", "z", "a"))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select nodes `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes(nodes = c(1, 3))
#'
#' # Verify that the node selection has been made
#' # using the `get_selection()` function
#' graph %>%
#'   get_selection()
#'
#' # Select nodes based on the node `type`
#' # being `z`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(
#'     conditions = type == "z")
#'
#' # Verify that an node selection has been made, and
#' # recall that the `3` and `4` nodes are of the
#' # `z` type
#' graph %>%
#'   get_selection()
#'
#' # Select edges based on the node value attribute
#' # being greater than 3.0 (first clearing the current
#' # selection of nodes)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(
#'     conditions = value > 3.0)
#'
#' # Verify that the correct node selection has been
#' # made; in this case, nodes `1` and `3` have values
#' # for `value` greater than 3.0
#' graph %>%
#'   get_selection()
#' @importFrom dplyr filter pull
#' @importFrom rlang enquo UQ get_expr
#' @export select_nodes

select_nodes <- function(graph,
                         conditions = NULL,
                         set_op = "union",
                         nodes = NULL) {

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
      "The graph contains no nodes, so, no selections can be made.",
      call. = FALSE)
  }

  # Stop function if `nodes` refers to node ID
  # values that are not in the graph
  if (!is.null(nodes)) {
    if (!any(nodes %in% graph$nodes_df$id)) {

      stop(
        "The values provided in `nodes` do not all correspond to node ID values in the graph.",
        call. = FALSE)
    }
  }

  # Capture provided conditions
  conditions <- rlang::enquo(conditions)

  # Create binding for a specific variable
  id <- NULL

  # Extract the graph's internal ndf
  nodes_df <- graph$nodes_df

  # Determine if there is an existing
  # selection of nodes
  existing_node_selection <-
    ifelse(
      graph_contains_node_selection(graph = graph), TRUE, FALSE)

  # Determine if there is an existing
  # selection of edges
  existing_edge_selection <-
    ifelse(
      graph_contains_edge_selection(graph = graph), TRUE, FALSE)

  # Get the existing node/edge count
  if (existing_node_selection | existing_edge_selection) {

    existing_selection_type <-
      ifelse(existing_node_selection, "node", "edge")

    existing_count <-
      suppressMessages(
        get_selection(graph)) %>% length()
  } else {

    existing_type <- as.character(NA)
    existing_count <- 0
  }

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `nodes_df`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    nodes_df <-
      filter(
        .data = nodes_df,
        rlang::UQ(conditions))
  }

  # Get the nodes as a vector
  nodes_selected <-
    nodes_df %>%
    dplyr::pull(id)

  # If a `nodes` vector provided, get the intersection
  # of that vector with the filtered node IDs
  if (!is.null(nodes)) {
    nodes_selected <- intersect(nodes, nodes_selected)
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  # Incorporate the selected nodes into the
  # graph's selection
  if (set_op == "union") {
    nodes_combined <- union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <- intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <- base::setdiff(nodes_prev_selection, nodes_selected)
  }

  # Add the node ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <-
    replace_graph_node_selection(
      graph = graph,
      replacement = nodes_combined)

  # Get the count of nodes in the selection
  new_count <- nrow(graph$node_selection)

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_nodes",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  # Construct message body
  if (existing_node_selection == FALSE &
      existing_edge_selection == FALSE) {
    msg_body <-
      glue::glue(
        "created a new selection of {new_count} nodes")

  } else if (existing_node_selection |
             existing_edge_selection) {

    if (existing_node_selection) {
      msg_body <-
        glue::glue(
          "modified an existing selection of {existing_count} nodes:
          -- {new_count} nodes are now in the active selection
          -- used the `{set_op}` set operation")
    }

    if (existing_edge_selection) {
      msg_body <-
        glue::glue(
          "created a new selection of {new_count} nodes:
          -- this replaces {existing_count} edges in prior selection")
    }
  }

  # Issue a message to the user
  emit_message(
    fcn_name = "select_nodes",
    message_body = msg_body)

  graph
}
