#' Mutate node attribute values for a selection of nodes
#'
#' Within a graph's internal node data frame (ndf), mutate node attribute values
#' only for nodes in a selection by using one or more expressions.
#'
#' This function makes use of an active selection of nodes (and the function
#' ending with `_ws` hints at this).
#'
#' Selections of nodes can be performed using the following node selection
#' (`select_*()`) functions: [select_nodes()], [select_last_nodes_created()],
#' [select_nodes_by_degree()], [select_nodes_by_id()], or
#' [select_nodes_in_neighborhood()].
#'
#' Selections of nodes can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out()], [trav_in()], [trav_both()],
#' [trav_out_node()], [trav_in_node()], [trav_out_until()], or
#' [trav_in_until()].
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
#' # and then select node `1`
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   set_node_attrs(
#'     node_attr = width,
#'     values = c(1.4, 0.3, 1.1)) %>%
#'   select_nodes(nodes = 1)
#'
#' # Get the graph's internal ndf
#' # to show which node attributes
#' # are available
#' graph %>% get_node_df()
#'
#' # Mutate the `width` node
#' # attribute for the nodes
#' # only in the active selection
#' # of nodes (node `1`); here,
#' # we divide each value in the
#' # selection by 2
#' graph <-
#'   graph %>%
#'   mutate_node_attrs_ws(
#'     width = width / 2)
#'
#' # Get the graph's internal
#' # ndf to show that the node
#' # attribute `width` was
#' # mutated only for node `1`
#' graph %>% get_node_df()
#'
#' # Create a new node attribute,
#' # called `length`, that is the
#' # log of values in `width` plus
#' # 2 (and, also, round all values
#' # to 2 decimal places)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_nodes(nodes = 2:3) %>%
#'   mutate_node_attrs_ws(
#'     length = (log(width) + 2) %>%
#'                round(2))
#'
#' # Get the graph's internal ndf
#' # to show that the node attribute
#' # values had been mutated only
#' # for nodes `2` and `3` (since
#' # node `1` is excluded, an NA
#' # value is applied)
#' graph %>% get_node_df()
#'
#' # Create a new node attribute
#' # called `area`, which is the
#' # product of the `width` and
#' # `length` attributes
#' graph <-
#'   graph %>%
#'   mutate_node_attrs_ws(
#'     area = width * length)
#'
#' # Get the graph's internal ndf
#' # to show that the node attribute
#' # values had been multiplied
#' # together (with new attr `area`)
#' # for nodes `2` and `3`
#' graph %>% get_node_df()
#'
#' # We can invert the selection
#' # and mutate node `1` several
#' # times to get an `area` value
#' # for that node
#' graph <-
#'   graph %>%
#'   invert_selection() %>%
#'   mutate_node_attrs_ws(
#'     length = (log(width) + 5) %>%
#'                round(2),
#'     area = width * length)
#'
#' # Get the graph's internal ndf
#' # to show that the 2 mutations
#' # occurred for node `1`, yielding
#' # non-NA values for its node
#' # attributes without changing
#' # those of the other nodes
#' graph %>% get_node_df()
#'
#' @import rlang
#' @export
mutate_node_attrs_ws <- function(graph,
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

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of nodes available.")
  }

  # Collect expressions
  exprs <- rlang::exprs(...)

  # Extract the graph's ndf
  ndf <- get_node_df(graph)

  # Stop function if any supplied
  # expressions mutate columns that
  # should not be changed
  if ("id" %in% names(exprs)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The variable `id` cannot undergo mutation")
  }

  # Determine which nodes are not
  # in the active selection
  unselected_nodes <-
    base::setdiff(
      get_node_ids(graph),
      suppressMessages(get_selection(graph)))

  for (i in 1:length(exprs)) {

    # Case where mutation occurs for an
    # existing node attribute
    if (names(exprs)[i] %in% colnames(ndf)) {

      ndf_replacement <-
        ndf %>%
        dplyr::mutate_(
          .dots = stats::setNames(list((exprs %>% paste())[i]),
                           names(exprs)[i]))

      ndf_replacement[
        which(ndf$id %in% unselected_nodes), ] <-
        ndf[
          which(ndf$id %in% unselected_nodes), ]

      # Update the graph's ndf
      graph$nodes_df <- ndf_replacement

      # Reobtain the changed ndf for
      # any subsequent mutations
      ndf <- get_node_df(graph)
    }

    # Case where mutation creates a
    # new node attribute
    if (!(names(exprs)[i] %in% colnames(ndf))) {

      ndf_replacement <-
        ndf %>%
        dplyr::mutate_(
          .dots = stats::setNames(list((exprs %>% paste())[i]),
                           names(exprs)[i]))

      ndf_replacement[
        which(ndf$id %in% unselected_nodes),
        which(colnames(ndf_replacement) == names(exprs)[i])] <- NA

      # Update the graph's ndf
      graph$nodes_df <- ndf_replacement

      # Reobtain the changed ndf for
      # any subsequent mutations
      ndf <- get_node_df(graph)
    }
  }

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
