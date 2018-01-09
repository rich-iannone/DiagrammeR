#' Mutate a set of node attribute values
#' @description Within a graph's internal node
#' data frame (ndf), mutate numeric node
#' attribute values using one or more expressions.
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
#' get_node_df(graph)
#' #>   id type label width
#' #> 1  1 <NA>     1   1.4
#' #> 2  2 <NA>     2   0.3
#' #> 3  3 <NA>     3   1.1
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
#' get_node_df(graph)
#' #>   id type label width
#' #> 1  1 <NA>     1  0.70
#' #> 2  2 <NA>     2  0.15
#' #> 3  3 <NA>     3  0.55
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
#' get_node_df(graph)
#' #>   id type label width length
#' #> 1  1 <NA>     1  0.70   1.64
#' #> 2  2 <NA>     2  0.15   0.10
#' #> 3  3 <NA>     3  0.55   1.40
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
#' get_node_df(graph)
#' #>   id type label width length  area
#' #> 1  1 <NA>     1  0.70   1.64 1.148
#' #> 2  2 <NA>     2  0.15   0.10 0.015
#' #> 3  3 <NA>     3  0.55   1.40 0.770
#' @importFrom dplyr mutate_
#' @importFrom rlang exprs
#' @export mutate_node_attrs

mutate_node_attrs <- function(graph,
                              ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, no node attributes can undergo mutation.",
      call. = FALSE)
  }

  # Collect expressions
  exprs <- rlang::exprs(...)

  # Extract the graph's ndf
  ndf <- get_node_df(graph)

  # Stop function if any supplied
  # expressions mutate columns that
  # should not be changed
  if ("id" %in% names(exprs)) {

    stop(
      "The variable `id` cannot undergo mutation.",
      call. = FALSE)
  }

  for (i in 1:length(exprs)) {
    ndf <-
      ndf %>%
      dplyr::mutate_(
        .dots = setNames(list((exprs %>% paste())[i]),
                         names(exprs)[i]))
  }

  # Update the graph
  graph$nodes_df <- ndf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "mutate_node_attrs",
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
