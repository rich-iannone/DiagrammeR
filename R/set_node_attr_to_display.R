#' Set the node attribute values to be rendered
#'
#' @description
#'
#' Set a node attribute type to display as node text when calling the
#' [render_graph()] function. This allows for display of different types of node
#' attribute values on a per-node basis. Without setting the `display`
#' attribute, rendering a graph will default to printing text from the `label`
#' attribute on nodes. Setting the `display` node attribute with this function
#' for the first time (i.e., the `display` column doesn't exist in the graph's
#' internal node data frame) will insert the `attr` value for all nodes
#' specified in `nodes` and a default value (`default`) for all remaining nodes.
#'
#' @inheritParams render_graph
#' @param attr The name of the attribute from which label text for the node will
#'   be obtained. If set to `NULL`, then `NA` values will be assigned to the
#'   `display` column for the chosen nodes.
#' @param nodes A length vector containing one or several node ID values (as
#'   integers) for which node attributes are set for display in the rendered
#'   graph. If `NULL`, all nodes from the graph are assigned the `display` value
#'   given as `attr`.
#' @param default The name of an attribute to set for all other graph nodes not
#'   included in `nodes`. This value only gets used if the `display` node
#'   attribute is not in the graph's internal node data frame.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 4,
#'     m = 4,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = c(2.5, 8.2, 4.2, 2.4))
#'
#' # For node ID values of `1`,
#' # `2`, and `3`, choose to display
#' # the node `value` attribute (for
#' # the other nodes, display nothing)
#' graph <-
#'   graph %>%
#'   set_node_attr_to_display(
#'     nodes = 1:3,
#'     attr = value,
#'     default = NA)
#'
#' # Show the graph's node data frame; the
#' # `display` node attribute will show for
#' # each row, which node attribute value to
#' # display when the graph is rendered
#' graph %>% get_node_df()
#'
#' # This function can be called multiple
#' # times on a graph; after the first time
#' # (i.e., creation of the `display`
#' # attribute), the `default` value won't
#' # be used
#' graph %>%
#'   set_node_attr_to_display(
#'     nodes = 4,
#'     attr = label) %>%
#'   set_node_attr_to_display(
#'     nodes = c(1, 3),
#'     attr = id) %>%
#'   get_node_df()
#'
#' @family node creation and removal
#'
#' @export
set_node_attr_to_display <- function(
    graph,
    attr = NULL,
    nodes = NULL,
    default = "label"
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Get the requested `attr`
  attr <-
    rlang::enquo(attr) %>% rlang::get_expr() %>% as.character()

  # If nothing provided for `attr`, set
  # it as NULL
  if (length(attr) == 0) {
    attr <- NULL
  }

  # Get the graph's node data frame as an object
  ndf <- graph$nodes_df

  # If `nodes` is NULL, assume that all nodes to
  # be assigned a `display` value
  if (is.null(nodes)) {
    nodes <- get_node_ids(graph)
  }

  # Stop function if any of the node ID values
  # provided in `nodes` do not exist in the graph
  if (!any(nodes %in% ndf$id)) {

    cli::cli_abort(
      "One or more node ID values in `nodes` are not present in the graph.")
  }

  # Stop function if the node attribute supplied as
  # `attr` does not exist in the ndf
  if (!is.null(attr) && !(attr %in% colnames(ndf))) {

    cli::cli_abort(
      "The node attribute given in `attr` is not in the graph's ndf.")
  }

  # If the `display` node attribute doesn't exist,
  # create that column and fill with the default value
  if (!("display" %in% colnames(ndf))) {
    ndf$display <- as.character(default)
  }

  # Create a tibble with the node ID values and the
  # requested node attribute to display
  if (is.null(attr)) {
    attr_to_display <-
      dplyr::tibble(
        id = as.integer(nodes),
        display = "is_na")

  } else {
    attr_to_display <-
      dplyr::tibble(
        id = as.integer(nodes),
        display = as.character(attr))

  }

  # Join the `attr_to_display` table with the `ndf`
  ndf <-
    ndf %>%
    dplyr::left_join(attr_to_display, by = "id")

  # Get the column numbers for the `.x`
  # and `.y` columns
  x_col <- grep("\\.x$", colnames(ndf))
  y_col <- grep("\\.y$", colnames(ndf))

  # Coalesce the 2 generated columns and create a
  # single-column data frame
  if (is.null(attr)) {
    display_col <-
      dplyr::coalesce(ndf[, y_col], ndf[, x_col])

    display_col <- dplyr::na_if(display_col, "is_na") %>%
      as.data.frame(stringsAsFactors = FALSE)

  } else {
    display_col <-
      dplyr::coalesce(ndf[, y_col], ndf[, x_col]) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }

  # Rename the column
  colnames(display_col)[1] <- "display"

  # Remove column numbers that end with ".x" or ".y"
  ndf <- ndf[-grep("\\.x$", colnames(ndf))]
  ndf <- ndf[-grep("\\.y$", colnames(ndf))]

  # Bind the `display_col` df to the `ndf` df and
  # modify the ordering of the columns
  ndf <-
    dplyr::bind_cols(ndf, display_col) %>%
    dplyr::relocate("id", "type", "label", "display")

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
