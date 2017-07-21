#' Apply a layout position to a single node
#' @description Apply position information for a
#' single node. This is done by setting the \code{x}
#' and \code{y} attrs for a node \code{id} or node
#' \code{label} supplied in \code{node}. When
#' rendering the graph, nodes with attribute values
#' set for \code{x} and \code{y} will be fixed to
#' those positions on the graph canvas.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node a single-length vector containing
#' either a node ID value (integer) or a node label
#' (character) for which position information should
#' be applied.
#' @param x the x coordinate to set for the node.
#' @param y the y coordinate to set for the node.
#' @param use_labels an option to use a node
#' \code{label} value in \code{node}. Note that this
#' is only possible if all nodes have distinct
#' \code{label} values set and none exist as an NA
#' value.
#' @return a graph object of class \code{dgr_graph}.
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
#' get_node_df(graph)
#' #>   id type label x y
#' #> 1  1 <NA>   one 1 1
#' #> 2  2 <NA>   two 2 2
#' #> 3  3 <NA> three 3 3
#' #> 4  4 <NA>  four 4 4
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
#' get_node_df(graph)
#' #>   id type label x y
#' #> 1  1 <NA>   one 1 4
#' #> 2  2 <NA>   two 3 3
#' #> 3  3 <NA> three 3 2
#' #> 4  4 <NA>  four 4 1
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
#' get_node_df(graph)
#' #>   id type label x y
#' #> 1  1 <NA>   one 1 1
#' #> 2  2 <NA>   two 2 2
#' #> 3  3 <NA> three 3 2
#' #> 4  4 <NA>  four 4 1
#' @importFrom dplyr case_when mutate coalesce
#' @export set_node_position

set_node_position <- function(graph,
                              node,
                              x,
                              y,
                              use_labels = FALSE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no node attributes can be set.")
  }

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Stop function if the node ID provided doesn't
  # exist in the graph
  if (use_labels == FALSE) {
    if (!(node %in% graph$nodes_df[, 1])) {
      stop("The node ID provided doesn't exist in the graph.")
    }
  }

  # If the `x` node attribute doesn't exist, create
  # that column in the `ndf`
  if (!("x" %in% colnames(ndf))) {
    ndf <-
      ndf %>%
      dplyr::mutate(x = as.numeric(NA))
  }

  # If the `y` node attribute doesn't exist, create
  # that column in the `ndf`
  if (!("y" %in% colnames(ndf))) {
    ndf <-
      ndf %>%
      dplyr::mutate(y = as.numeric(NA))
  }

  if (use_labels == TRUE) {

    # Ensure that the label column contains unique,
    # non-NA values
    unique_labels_available <-
      is_attr_unique_and_non_na(
        graph = graph,
        which_graph_df = "ndf",
        attr = "label")

    # Stop function if `label` doesn't contain
    # unique, non-NA values
    if (unique_labels_available == FALSE) {
      stop("The `label` attribute in the graph's ndf must contain unique, non-NA values.")
    }

    # Use `case_when` statements to selectively perform
    # a vectorized `if` statement across all nodes for
    # the `x` and `y` node attribute
    x_attr_new <-
      dplyr::case_when(
        ndf$label == node ~ x,
        TRUE ~ as.numeric(ndf$x))

    y_attr_new <-
      dplyr::case_when(
        ndf$label == node ~ y,
        TRUE ~ as.numeric(ndf$y))
  }

  if (use_labels == FALSE) {

    # Use `case_when` statements to selectively perform
    # a vectorized `if` statement across all nodes for
    # the `x` and `y` node attribute
    x_attr_new <-
      dplyr::case_when(
        ndf$id == node ~ x,
        TRUE ~ as.numeric(ndf$x))

    y_attr_new <-
      dplyr::case_when(
        ndf$id == node ~ y,
        TRUE ~ as.numeric(ndf$y))
  }

  # Replace the `x` column to the ndf with a
  # coalesced version of the column contents
  ndf$x <- dplyr::coalesce(x_attr_new, ndf$x)

  # Replace the `y` column to the ndf with a
  # coalesced version of the column contents
  ndf$y <- dplyr::coalesce(y_attr_new, ndf$y)

  # Replace the graph's node data frame with `ndf`
  graph$nodes_df <- ndf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_node_position",
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
