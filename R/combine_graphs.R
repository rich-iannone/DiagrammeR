#' Combine two graphs into a single graph
#'
#' @description
#'
#' Combine two graphs in order to make a new graph.
#'
#' @param x A `DiagrammeR` graph object to which another graph will be unioned.
#'   This graph should be considered the graph from which global graph
#'   attributes will be inherited in the resulting graph.
#' @param y A `DiagrammeR` graph object that is to be unioned with the graph
#'   supplied as `x`.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with a cycle
#' # containing 6 nodes
#' graph_cycle <-
#'  create_graph() %>%
#'    add_cycle(n = 6)
#'
#' # Create a random graph with
#' # 8 nodes and 15 edges using the
#' # `add_gnm_graph()` function
#' graph_random <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 8,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Combine the two graphs in a
#' # union operation
#' combined_graph <-
#'   combine_graphs(
#'     graph_cycle,
#'     graph_random)
#'
#' # Get the number of nodes in
#' # the combined graph
#' combined_graph %>% count_nodes()
#'
#' # The `combine_graphs()`
#' # function will renumber
#' # node ID values in graph `y`
#' # during the union; this ensures
#' # that node ID values are unique
#' combined_graph %>% get_node_ids()
#'
#' @export
combine_graphs <- function(
    x,
    y
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object `x` is valid
  check_graph_valid(x)

  # Validation: Graph object `y` is valid
  if (!graph_object_valid(y)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object supplied to `y` is not valid.")
  }

  # Get the number of nodes ever created for
  # graph `x`
  nodes_created <- x$last_node

  # Get the number of nodes in the graph
  nodes_graph_1 <- x %>% count_nodes()

  # Get the number of edges ever created for
  # graph `x`
  edges_created <- x$last_edge

  # Get the number of edges in the graph
  edges_graph_1 <- x %>% count_edges()

  # Get the node data frame for graph `x`
  x_nodes_df <- get_node_df(x)

  # Get the node data frame for graph `y`
  y_nodes_df <- get_node_df(y)

  # Is label a copy of node IDs in graph `y`?
  if (all(as.character(y_nodes_df[, 1]) == y_nodes_df[, 3]) &
      !any(is.na(y_nodes_df[, 3]))) {
    y_label_node <- TRUE
  } else {
    y_label_node <- FALSE
  }

  # Add a new node attribute `new_node_id`
  y_nodes_df$new_node_id <-
    seq(nodes_created + 1,
        nodes_created + nrow(y_nodes_df))

  # Get the edge data frame for graph `x`
  x_edges_df <- get_edge_df(x)

  # Get the edge data frame for graph `y`
  y_edges_df <- get_edge_df(y)

  y_edges_df <-
    dplyr::inner_join(
      y_edges_df,
      y_nodes_df,
      by = c("from" = "id")) %>%
    dplyr::rename(from_new = "new_node_id") %>%
    dplyr::select(-"type", -"label")

  # Rename `id` if it has a `.x` suffix
  if ("id.x" %in% colnames(y_edges_df)) {

    y_edges_df <-
      y_edges_df %>%
      dplyr::rename(id = "id.x")
  }

  y_edges_df <-
    dplyr::inner_join(
      y_edges_df,
      y_nodes_df,
      by = c("to" = "id")) %>%
    dplyr::rename(to_new = "new_node_id") %>%
    dplyr::select(-"type", -"label")

  # Rename `id` if it has a `.x` suffix
  if ("id.x" %in% colnames(y_edges_df)) {

    y_edges_df <-
      y_edges_df %>%
      dplyr::rename(id = "id.x")
  }

  # Copy new node IDs to `from` and `to` edge attrs
  y_edges_df$from <- y_edges_df$from_new
  y_edges_df$to <- y_edges_df$to_new

  # Remove columns ending with `.x` or `_new`
  y_edges_df <-
    y_edges_df %>%
    dplyr::select(
      !dplyr::ends_with(c(".x", "_new")))

  # Rename column names with `.y` suffixes
  colnames(y_edges_df) <-
    gsub(".y$", "", colnames(y_edges_df))

  y_edges_df$id <- y_edges_df$id + edges_created

  # Copy new node IDs to `nodes` node attr
  y_nodes_df$id <- y_nodes_df$new_node_id

  # Remove the last column from `y_nodes_df`
  y_nodes_df <-
    y_nodes_df[, -ncol(y_nodes_df)]

  # If label is a copy of node ID in graph `y`,
  # rewrite labels to match new node ID values
  if (y_label_node) {
    y_nodes_df[, 3] <- as.character(y_nodes_df[, 1])
  }

  # Combine the node data frames for both graphs
  combined_nodes <-
    dplyr::bind_rows(x_nodes_df, y_nodes_df)

  # Combine the edge data frames for both graphs
  combined_edges <-
    dplyr::bind_rows(x_edges_df, y_edges_df)

  # Modify the graph object and inherit attributes
  # from the first graph provided (`x`)
  x$nodes_df <- combined_nodes
  x$edges_df <- combined_edges
  x$directed <-
    ifelse(
      !is_graph_directed(x) ||
        !is_graph_directed(y),
      FALSE, TRUE)
  x$last_node <- nrow(combined_nodes)
  x$last_edge <- nrow(combined_edges)

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- x %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_added <- nodes_graph_2 - nodes_graph_1

  # Get the updated number of edges in the graph
  edges_graph_2 <- x %>% count_edges()

  # Get the number of edges added to
  # the graph
  edges_added <- edges_graph_2 - edges_graph_1

  # Update the `graph_log` df with an action
  x$graph_log <-
    add_action_to_log(
      graph_log = x$graph_log,
      version_id = nrow(x$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(x$nodes_df),
      edges = nrow(x$edges_df),
      d_n = nodes_added,
      d_e = edges_added)

  # Write graph backup if the option is set
  if (x$graph_info$write_backups) {
    save_graph_as_rds(graph = x)
  }

  x
}
