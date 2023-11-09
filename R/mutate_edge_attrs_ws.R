#' Mutate edge attribute values for a selection of edges
#'
#' @description
#'
#' Within a graph's internal edge data frame (edf), mutate edge attribute values
#' only for edges in a selection by using one or more expressions.
#'
#' This function makes use of an active selection of edges (and the function
#' ending with `_ws` hints at this).
#'
#' Selections of edges can be performed using the following selection
#' (`select_*()`) functions: [select_edges()], [select_last_edges_created()],
#' [select_edges_by_edge_id()], or [select_edges_by_node_id()].
#'
#' Selections of edges can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out_edge()], [trav_in_edge()],
#' [trav_both_edge()], or [trav_reverse_edge()].
#'
#' @inheritParams render_graph
#' @param ... Expressions used for the mutation of edge attributes. LHS of each
#'   expression is either an existing or new edge attribute name. The RHS can
#'   consist of any valid R code that uses edge attributes as variables.
#'   Expressions are evaluated in the order provided, so, edge attributes
#'   created or modified are ready to use in subsequent expressions.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph with 3 edges
#' # and then select edge `1`
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 4) %>%
#'   set_edge_attrs(
#'     edge_attr = width,
#'     values = c(3.4, 2.3, 7.2)) %>%
#'   select_edges(edges = 1)
#'
#' # Get the graph's internal edf
#' # to show which edge attributes
#' # are available
#' graph %>% get_edge_df()
#'
#' # Mutate the `width` edge
#' # attribute for the edges
#' # only in the active selection
#' # of edges (edge `1`); here,
#' # we divide each value in the
#' # selection by 2
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs_ws(
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
#'   clear_selection() %>%
#'   select_edges(edges = 2:3) %>%
#'   mutate_edge_attrs_ws(
#'     length = (log(width) + 2) %>%
#'                round(2))
#'
#' # Get the graph's internal edf
#' # to show that the edge attribute
#' # values had been mutated only
#' # for edges `2` and `3` (since
#' # edge `1` is excluded, an NA
#' # value is applied)
#' graph %>% get_edge_df()
#'
#' # Create a new edge attribute
#' # called `area`, which is the
#' # product of the `width` and
#' # `length` attributes
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs_ws(
#'     area = width * length)
#'
#' # Get the graph's internal edf
#' # to show that the edge attribute
#' # values had been multiplied
#' # together (with new attr `area`)
#' # for nodes `2` and `3`
#' graph %>% get_edge_df()
#'
#' # We can invert the selection
#' # and mutate edge `1` several
#' # times to get an `area` value
#' # for that edge
#' graph <-
#'   graph %>%
#'   invert_selection() %>%
#'   mutate_edge_attrs_ws(
#'     length = (log(width) + 5) %>%
#'                round(2),
#'     area = width * length)
#'
#' # Get the graph's internal edf
#' # to show that the 2 mutations
#' # occurred for edge `1`, yielding
#' # non-NA values for its edge
#' # attributes without changing
#' # those of the other edges
#' graph %>% get_edge_df()
#'
#' @family edge creation and removal
#'
#' @export
mutate_edge_attrs_ws <- function(
    graph,
    ...
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Graph object has valid edge selection
  check_graph_contains_edge_selection(graph)

  # Collect expressions
  exprs <- rlang::exprs(...)

  # Extract the graph's edf
  edf <- get_edge_df(graph)

  # Stop function if any supplied
  # expressions mutate columns that
  # should not be changed
  if ("id" %in% names(exprs) ||
      "from" %in% names(exprs) ||
      "to" %in% names(exprs)) {

    cli::cli_abort(
      "The variables `id`, `from`, or `to` cannot undergo mutation.")
  }

  # Determine which edges are not
  # in the active selection
  unselected_edges <-
    base::setdiff(
      get_edge_ids(graph),
      suppressMessages(get_selection(graph)))

  s <- edf$id %in% unselected_edges
  order <- c(which(!s), which(s))

  edf <-
    dplyr::bind_rows(
      edf %>% dplyr::filter(!(!!enquo(s))) %>% dplyr::mutate(!!!enquos(...)),
      edf %>% dplyr::filter( (!!enquo(s)))
    ) %>%
    dplyr::arrange(!!enquo(order))

  graph$edges_df <- edf

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
