#' Mutate node attribute values for a selection of nodes
#' @description Within a graph's internal node
#' data frame (ndf), mutate node attribute
#' values only for nodes in a selection by
#' using one or more expressions.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param ... expressions used for the mutation
#' of node attributes. LHS of each expression is
#' either an existing or new node attribute name.
#' The RHS can consist of any valid R code that
#' uses node attributes as variables. Expressions
#' are evaluated in the order provided, so, node
#' attributes created or modified are ready to
#' use in subsequent expressions.
#' @return a graph object of class
#' \code{dgr_graph}.
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
#' get_node_df(graph)
#' #>   id type label width
#' #> 1  1 <NA>     1   1.4
#' #> 2  2 <NA>     2   0.3
#' #> 3  3 <NA>     3   1.1
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
#' get_node_df(graph)
#' #>   id type label width
#' #> 1  1 <NA>     1  0.7
#' #> 2  2 <NA>     2  0.3
#' #> 3  3 <NA>     3  1.1
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
#' get_node_df(graph)
#' #>   id type label width length
#' #> 1  1 <NA>     1   0.7     NA
#' #> 2  2 <NA>     2   0.3    0.8
#' #> 3  3 <NA>     3   1.1    2.1
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
#' get_node_df(graph)
#' #>   id type label width length area
#' #> 1  1 <NA>     1   0.7     NA   NA
#' #> 2  2 <NA>     2   0.3    0.8 0.24
#' #> 3  3 <NA>     3   1.1    2.1 2.31
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
#' get_node_df(graph)
#' #>   id type label width length  area
#' #> 1  1 <NA>     1   0.7   4.64 3.248
#' #> 2  2 <NA>     2   0.3   0.80 0.240
#' #> 3  3 <NA>     3   1.1   2.10 2.310
#' @importFrom dplyr mutate_
#' @importFrom rlang exprs
#' @export mutate_node_attrs_ws

mutate_node_attrs_ws <- function(graph,
                                 ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no node attributes can undergo mutation.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes available.")
  }

  # Collect expressions
  exprs <- rlang::exprs(...)

  # Extract the graph's ndf
  ndf <- get_node_df(graph)

  # Stop function if any supplied
  # expressions mutate columns that
  # should not be changed
  if ("id" %in% names(exprs)) {
    stop("The variable `id` cannot undergo mutation.")
  }

  # Determine which nodes are not
  # in the active selection
  unselected_nodes <-
    base::setdiff(get_node_ids(graph), get_selection(graph))

  for (i in 1:length(exprs)) {

    # Case where mutation occurs for an
    # existing node attribute
    if (names(exprs)[i] %in% colnames(ndf)) {

      ndf_replacement <-
        ndf %>%
        dplyr::mutate_(
          .dots = setNames(list((exprs %>% paste())[i]),
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
          .dots = setNames(list((exprs %>% paste())[i]),
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
      function_used = "mutate_node_attrs_ws",
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
