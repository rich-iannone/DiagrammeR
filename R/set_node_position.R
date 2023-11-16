#' Apply a layout position to a single node
#'
#' @description
#'
#' Apply position information for a single node. This is done by setting the `x`
#' and `y` attrs for a node `id` or node `label` supplied in `node`. When
#' rendering the graph, nodes with attribute values set for `x` and `y` will be
#' fixed to those positions on the graph canvas.
#'
#' @inheritParams render_graph
#' @param node A single-length vector containing either a node ID value
#'   (integer) or a node label (character) for which position information should
#'   be applied.
#' @param x The x coordinate to set for the node.
#' @param y The y coordinate to set for the node.
#' @param use_labels An option to use a node `label` value in `node`. Note that
#'   this is only possible if all nodes have distinct `label` values set and
#'   none exist as an NA value.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a simple graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(label = "one") %>%
#'   add_node(label = "two") %>%
#'   add_node(label = "three") %>%
#'   add_node(label = "four")
#'
#' # Add position information to each of
#' # the graph's nodes
#' graph <-
#'   graph %>%
#'   set_node_position(
#'     node = 1,
#'     x = 1, y = 1) %>%
#'   set_node_position(
#'     node = 2,
#'     x = 2, y = 2) %>%
#'   set_node_position(
#'     node = 3,
#'     x = 3, y = 3) %>%
#'   set_node_position(
#'     node = 4,
#'     x = 4, y = 4)
#'
#' # View the graph's node data frame to
#' # verify that the `x` and `y` node
#' # attributes are available and set to
#' # the values provided
#' graph %>% get_node_df()
#'
#' # The same function can modify the data
#' # in the `x` and `y` attributes
#' graph <-
#'   graph %>%
#'   set_node_position(
#'     node = 1,
#'     x = 1, y = 4) %>%
#'   set_node_position(
#'     node = 2,
#'     x = 3, y = 3) %>%
#'   set_node_position(
#'     node = 3,
#'     x = 3, y = 2) %>%
#'   set_node_position(
#'     node = 4,
#'     x = 4, y = 1)
#'
#' # View the graph's node data frame
#' graph %>% get_node_df()
#'
#' # Position changes can also be made by
#' # supplying a node `label` value (and setting
#' # `use_labels` to TRUE). For this to work,
#' # all `label` values in the graph's ndf must
#' # be unique and non-NA
#' graph <-
#'   graph %>%
#'   set_node_position(
#'     node = "one",
#'     x = 1, y = 1,
#'     use_labels = TRUE) %>%
#'   set_node_position(
#'     node = "two",
#'     x = 2, y = 2,
#'     use_labels = TRUE)
#'
#' # View the graph's node data frame
#' graph %>% get_node_df()
#'
#' @family node creation and removal
#'
#' @export
set_node_position <- function(
    graph,
    node,
    x,
    y,
    use_labels = FALSE
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Stop function if the node ID provided doesn't
  # exist in the graph
  if (!use_labels && !(node %in% graph$nodes_df[, 1])) {

    cli::cli_abort(
      "The node ID provided doesn't exist in the graph.")
  }

  # If the `x` node attribute doesn't exist, create
  # that column in the `ndf`
  if (!("x" %in% colnames(ndf))) {
    ndf$x <- NA_real_
  }

  # If the `y` node attribute doesn't exist, create
  # that column in the `ndf`
  if (!("y" %in% colnames(ndf))) {
    ndf$y <- NA_real_
  }

  if (use_labels) {

    # Ensure that the label column contains unique,
    # non-NA values
    unique_labels_available <-
      is_attr_unique_and_non_na(
        graph = graph,
        which_graph_df = "ndf",
        attr = "label")

    # Stop function if `label` doesn't contain
    # unique, non-NA values
    if (!unique_labels_available) {

      cli::cli_abort(
        "The `label` attribute in the graph's ndf must contain unique, non-NA values.")
    }

    # Use `case_when` statements to selectively perform
    # a vectorized `if` statement across all nodes for
    # the `x` and `y` node attribute
    x_attr_new <-
      dplyr::case_when(
        ndf$label == node ~ x,
        .default = as.numeric(ndf$x))

    y_attr_new <-
      dplyr::case_when(
        ndf$label == node ~ y,
        .default = as.numeric(ndf$y))
  }

  if (!use_labels) {

    # Use `case_when` statements to selectively perform
    # a vectorized `if` statement across all nodes for
    # the `x` and `y` node attribute
    x_attr_new <-
      dplyr::case_when(
        ndf$id == node ~ x,
        .default = as.numeric(ndf$x))

    y_attr_new <-
      dplyr::case_when(
        ndf$id == node ~ y,
        .default = as.numeric(ndf$y))
  }

  # Replace the `x` column to the ndf with a
  # coalesced version of the column contents
  ndf$x <- dplyr::coalesce(x_attr_new, ndf$x)

  # Replace the `y` column to the ndf with a
  # coalesced version of the column contents
  ndf$y <- dplyr::coalesce(y_attr_new, ndf$y)

  # Replace the graph's node data frame with `ndf`
  graph$nodes_df <- ndf

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
      function_used = fcn_name,
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
