#' Copy a node attribute column and set the name
#'
#' @description
#'
#' Within a graph's internal node data frame (ndf), copy the contents an
#' existing node attribute and create a distinct node attribute within the ndf
#' with a different attribute name.
#'
#' @inheritParams render_graph
#' @param node_attr_from The name of the node attribute column from which values
#'   will be copied.
#' @param node_attr_to The name of the new node attribute column to which the
#'   copied values will be placed.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5,
#'     m = 10,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = shape,
#'     values = "circle") %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = rnorm(
#'       n = count_nodes(.),
#'       mean = 5,
#'       sd = 1) %>% round(1))
#'
#' # Get the graph's internal
#' # ndf to show which node
#' # attributes are available
#' graph %>% get_node_df()
#'
#' # Make a copy the `value`
#' # node attribute as the
#' # `width` node attribute
#' graph <-
#'   graph %>%
#'   copy_node_attrs(
#'     node_attr_from = value,
#'     node_attr_to = size)
#'
#' # Get the graph's internal
#' # ndf to show that the node
#' # attribute had been copied
#' graph %>% get_node_df()
#'
#' @family node creation and removal
#'
#' @export
copy_node_attrs <- function(
    graph,
    node_attr_from,
    node_attr_to
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Get the requested `node_attr_from`
  node_attr_from <-
    rlang::enquo(node_attr_from) %>% rlang::get_expr() %>% as.character()

  # Get the requested `node_attr_to`
  node_attr_to <-
    rlang::enquo(node_attr_to) %>% rlang::get_expr() %>% as.character()

  # Stop function if `node_attr_from` and
  # `node_attr_to` are identical
  if (node_attr_from == node_attr_to) {

    cli::cli_abort(
      "You cannot use make a copy with the same name.")
  }

  # Stop function if `node_attr_to` is `nodes` or `node`
  if (any(c("nodes", "node") %in% node_attr_to)) {

    cli::cli_abort(
      "You cannot use `nodes` or `node` as names.")
  }

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's column
  if (!any(column_names_graph %in% node_attr_from)) {

    cli::cli_abort(
      "The node attribute to copy is not in the ndf.")
  }

  # Get the column number for the node attr to copy
  col_num_copy_from <-
    which(colnames(nodes) %in% node_attr_from)

  # Copy the column using `bind_cols()`
  nodes <-
    dplyr::bind_cols(
      nodes,
      as.data.frame(
        nodes[, col_num_copy_from],
        stringsAsFactors = FALSE))

  # Set the column name for the copied attr
  colnames(nodes)[ncol(nodes)] <- node_attr_to

  # Modify the new graph object
  graph$nodes_df <- nodes

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
