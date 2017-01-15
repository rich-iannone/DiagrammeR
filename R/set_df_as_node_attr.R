#' Set a data frame as a node attribute
#' @description From a graph object of class
#' \code{dgr_graph}, bind a data frame as a node
#' attribute property for one given graph node.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node the node ID to which the data frame
#' will be bound as an attribute.
#' @param df the data frame to be bound to the
#' node.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Create a simple data frame to add as
#' # a node attribute
#' df <-
#'   tibble(
#'     a = c("one", "two", "three"),
#'     b = c(1, 2, 3))
#'
#' # Bind the data frame as a node attribute
#' # of node `1`
#' graph <-
#'   set_df_as_node_attr(
#'     graph = graph,
#'     node = 1,
#'     df = df)
#' @importFrom tidyr nest
#' @importFrom dplyr everything bind_rows
#' @importFrom tibble tibble as_tibble
#' @export set_df_as_node_attr

set_df_as_node_attr <- function(graph,
                                node,
                                df) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, a df cannot be added.")
  }

  # Value given for node must only be a single value
  if (length(node) > 1) {
    stop("Only one node can be specified.")
  }

  # Value given for node must correspond to a node ID
  # in the graph
  if (!(node %in% graph$nodes_df$id)) {
    stop("The value given for `node` does not correspond to a node ID.")
  }

  # Create nested data frame using `df` in a list column
  df_nested <- tidyr::nest(df, dplyr::everything())

  # Create the `df_storage` tibble
  df_storage <-
    data.frame(
      df_id = as.character(NA),
      df_data = as.character(NA),
      stringsAsFactors = FALSE)[-1, ] %>%
    as_tibble()

  # Generate a random 8-character, alphanumeric
  # string to use as a data frame ID (`df_id``)
  df_id <-
    replicate(
      8, sample(c(LETTERS, letters, 0:9), 1)) %>%
    paste(collapse = "")

  # Generate row with table
  df_row <- tibble(df_id = df_id) %>% mutate(df_data = df_nested)

  # Bind the new row with `df_id` and `df_data` to `df_storage`
  df_storage <- suppressWarnings(rbind(df_storage, df_row))

  # Bind the `df_storage` tibble to an existing `df_storage` object
  # if it exists in the graph
  if (!is.null(graph$df_storage)) {
    graph$df_storage <-
      dplyr::bind_rows(graph$df_storage, df_storage)
  } else {
    graph$df_storage <- df_storage
  }

  # Set the `_data_frame_` node attribute using the
  # `set_node_attrs()` function
  graph <-
    set_node_attrs(
      x = graph,
      node_attr = "_data_frame_",
      values = df_id,
      nodes = node)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_df_as_node_attr",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
