#' Rescale numeric node attribute values
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, take a set of numeric values for a
#' node attribute, rescale to a new numeric or color range, then write to the
#' same node attribute or to a new node attribute column.
#'
#' @inheritParams render_graph
#' @param node_attr_from The node attribute containing numeric data that is to
#'   be rescaled to new numeric or color values.
#' @param to_lower_bound The lower bound value for the set of rescaled values.
#'   This can be a numeric value or an X11 color name.
#' @param to_upper_bound The upper bound value for the set of rescaled values.
#'   This can be a numeric value or an X11 color name.
#' @param node_attr_to An optional name of a new node attribute to which the
#'   recoded values will be applied. This will retain the original node
#'   attribute and its values.
#' @param from_lower_bound An optional, manually set lower bound value for the
#'   rescaled values. If not set, the minimum value from the set will be used.
#' @param from_upper_bound An optional, manually set upper bound value for the
#'   rescaled values. If not set, the minimum value from the set will be used.
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
#'     node_attr = value,
#'     values = rnorm(
#'       n = count_nodes(.),
#'       mean = 5,
#'       sd = 1) %>% round(1))
#'
#' # Get the graph's internal ndf
#' # to show which node attributes
#' # are available
#' graph %>% get_node_df()
#'
#' # Rescale the `value` node
#' # attribute, so that its values
#' # are rescaled between 0 and 1
#' graph <-
#'   graph %>%
#'   rescale_node_attrs(
#'     node_attr_from = value,
#'     to_lower_bound = 0,
#'     to_upper_bound = 1)
#'
#' # Get the graph's internal ndf
#' # to show that the node attribute
#' # values had been rescaled
#' graph %>% get_node_df()
#'
#' # Scale the values in the `value`
#' # node attribute to different
#' # shades of gray for the `fillcolor`
#' # and `fontcolor` node attributes
#' graph <-
#'   graph %>%
#'   rescale_node_attrs(
#'     node_attr_from = value,
#'     to_lower_bound = "gray80",
#'     to_upper_bound = "gray20",
#'     node_attr_to = fillcolor) %>%
#'   rescale_node_attrs(
#'     node_attr_from = value,
#'     to_lower_bound = "gray5",
#'     to_upper_bound = "gray95",
#'     node_attr_to = fontcolor)
#'
#' # Get the graph's internal ndf
#' # once more to show that scaled
#' # grayscale colors are now available
#' # in the `fillcolor` and `fontcolor`
#' # node attributes
#' graph %>% get_node_df()
#'
#' @family node creation and removal
#'
#' @export
rescale_node_attrs <- function(
    graph,
    node_attr_from,
    to_lower_bound = 0,
    to_upper_bound = 1,
    node_attr_to = NULL,
    from_lower_bound = NULL,
    from_upper_bound = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Get the requested `node_attr_from`
  node_attr_from <-
    rlang::enquo(node_attr_from) %>% rlang::get_expr() %>% as.character()

  # Get the requested `node_attr_to`
  node_attr_to <-
    rlang::enquo(node_attr_to) %>% rlang::get_expr() %>% as.character()

  if (length(node_attr_to) == 0) {
    node_attr_to <- NULL
  }

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's node attributes
  if (!any(column_names_graph %in% node_attr_from)) {

    cli::cli_abort(
      "The node attribute to rescale is not in the ndf.")
  }

  # Extract the vector to rescale from the `nodes` df
  vector_to_rescale <-
    nodes %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(node_attr_from),
        .fns = as.numeric)
      ) %>%
    dplyr::pull(var = !!node_attr_from)

  # TODO condition could be simplified to
  # is.null(from_lower) || (!is.null(from_lower) && is.null(from_upper))?
  if ((!is.null(from_lower_bound) &&
       is.null(from_upper_bound)) ||
      (is.null(from_lower_bound) &&
       !is.null(from_upper_bound)) ||
      (is.null(from_lower_bound) &&
       is.null(from_upper_bound))) {

    from <- range(vector_to_rescale, na.rm = TRUE, finite = TRUE)

  } else {
    from <- c(from_lower_bound, from_upper_bound)
  }

  # Get vector of rescaled, numeric node
  # attribute values
  if (is.numeric(to_lower_bound) &&
      is.numeric(to_upper_bound)) {

    nodes_attr_vector_rescaled <-
      round(
        scales::rescale(
          x = vector_to_rescale,
          to = c(to_lower_bound,
                 to_upper_bound),
          from = from),
        3)
  }

  # Get vector of rescaled, node attribute color values
  if ((to_lower_bound %in% grDevices::colors()) &&
      (to_upper_bound %in% grDevices::colors())) {

    nodes_attr_vector_rescaled <-
      scales::cscale(
        x = vector_to_rescale,
        palette = scales::seq_gradient_pal(
          to_lower_bound,
          to_upper_bound))
  }

  # If a new node attribute name was not provided,
  # overwrite the source node attribute with the
  # rescaled values
  if (is.null(node_attr_to)) {
    node_attr_to <- node_attr_from
  }

  node_attr_to_2 <- rlang::enquo(node_attr_to)

  # Set the node attribute values for nodes specified
  # in selection
  graph <-
    set_node_attrs(
      graph = graph,
      node_attr = !!node_attr_to_2,
      values = nodes_attr_vector_rescaled
    )

  # Remove last action from the `graph_log`
  graph$graph_log <- graph$graph_log[1:(nrow(graph$graph_log) - 1), ]

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
