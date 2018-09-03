#' Mutate a set of edge attribute values
#'
#' Within a graph's internal edge data frame (edf), mutate numeric edge
#'   attribute values using one or more expressions.
#' @inheritParams render_graph
#' @param ... expressions used for the mutation of edge attributes. LHS of each
#'   expression is either an existing or new edge attribute name. The RHS can
#'   consist of any valid R code that uses edge attributes as variables.
#'   Expressions are evaluated in the order provided, so, edge attributes
#'   created or modified are ready to use in subsequent expressions.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 3 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 4) %>%
#'   set_edge_attrs(
#'     edge_attr = width,
#'     values = c(3.4, 2.3, 7.2))
#'
#' # Get the graph's internal edf
#' # to show which edge attributes
#' # are available
#' graph %>% get_edge_df()
#'
#' # Mutate the `width` edge
#' # attribute, dividing each
#' # value by 2
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     width = width / 2)
#'
#' # Get the graph's internal
#' # edf to show that the edge
#' # attribute `width` had its
#' # values changed
#' graph %>% get_edge_df()
#'
#' # Create a new edge attribute,
#' # called `length`, that is the
#' # log of values in `width` plus
#' # 2 (and, also, round all values
#' # to 2 decimal places)
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     length = (log(width) + 2) %>%
#'                round(2))
#'
#' # Get the graph's internal edf
#' # to show that the edge attribute
#' # values had been mutated
#' graph %>% get_edge_df()
#'
#' # Create a new edge attribute
#' # called `area`, which is the
#' # product of the `width` and
#' # `length` attributes
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     area = width * length)
#'
#' # Get the graph's internal edf
#' # to show that the edge attribute
#' # values had been multiplied
#' # together (with new attr `area`)
#' graph %>% get_edge_df()
#' @importFrom dplyr mutate_
#' @importFrom rlang exprs
#' @export
mutate_edge_attrs <- function(graph,
                              ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Collect expressions
  exprs <- rlang::exprs(...)

  # Extract the graph's edf
  edf <- get_edge_df(graph)

  # Stop function if any supplied
  # expressions mutate columns that
  # should not be changed
  if ("id" %in% names(exprs) |
      "from" %in% names(exprs) |
      "to" %in% names(exprs)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The variables `id`, `from`, or `to` cannot undergo mutation")
  }

  for (i in 1:length(exprs)) {
    edf <-
      edf %>%
      dplyr::mutate_(
        .dots = setNames(list((exprs %>% paste())[i]),
                         names(exprs)[i]))
  }

  # Update the graph
  graph$edges_df <- edf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
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
