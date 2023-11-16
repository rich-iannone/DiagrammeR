#' Mutate a set of node attribute values
#'
#' @description
#'
#' Within a graph's internal node data frame (ndf), mutate numeric node
#' attribute values using one or more expressions.
#'
#' @inheritParams render_graph
#' @param ... Expressions used for the mutation of node attributes. LHS of each
#'   expression is either an existing or new node attribute name. The RHS can
#'   consist of any valid R code that uses node attributes as variables.
#'   Expressions are evaluated in the order provided, so, node attributes
#'   created or modified are ready to use in subsequent expressions.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with 3 nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   set_node_attrs(
#'     node_attr = width,
#'     values = c(1.4, 0.3, 1.1))
#'
#' # Get the graph's internal ndf
#' # to show which node attributes
#' # are available
#' graph %>% get_node_df()
#'
#' # Mutate the `width` node
#' # attribute, dividing each
#' # value by 2
#' graph <-
#'   graph %>%
#'   mutate_node_attrs(
#'     width = width / 2)
#'
#' # Get the graph's internal
#' # ndf to show that the node
#' # attribute `width` had its
#' # values changed
#' graph %>% get_node_df()
#'
#' # Create a new node attribute,
#' # called `length`, that is the
#' # log of values in `width` plus
#' # 2 (and, also, round all values
#' # to 2 decimal places)
#' graph <-
#'   graph %>%
#'   mutate_node_attrs(
#'     length = (log(width) + 2) %>%
#'                round(2))
#'
#' # Get the graph's internal ndf
#' # to show that the node attribute
#' # values had been mutated
#' graph %>% get_node_df()
#'
#' # Create a new node attribute
#' # called `area`, which is the
#' # product of the `width` and
#' # `length` attributes
#' graph <-
#'   graph %>%
#'   mutate_node_attrs(
#'     area = width * length)
#'
#' # Get the graph's internal ndf
#' # to show that the node attribute
#' # values had been multiplied
#' # together (with new attr `area`)
#' graph %>% get_node_df()
#'
#' @family node creation and removal
#'
#' @export
mutate_node_attrs <- function(
    graph,
    ...
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Collect expressions
  exprs <- rlang::exprs(...)

  # Extract the graph's ndf
  ndf <- get_node_df(graph)

  # Stop function if any supplied
  # expressions mutate columns that
  # should not be changed
  if ("id" %in% names(exprs)) {
    cli::cli_abort("The variable `id` cannot undergo mutation.")
  }

  ndf <- ndf %>% dplyr::mutate(!!!enquos(...))

  # Update the graph
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
