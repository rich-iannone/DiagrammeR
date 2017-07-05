#' Trigger the execution of a series of graph actions
#' @description Execute the graph actions stored in
#' the graph through the use of the
#' \code{add_graph_action()} function. These actions
#' will be invoked in order and any errors
#' encountered will trigger a warning message and
#' result in no change to the input graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param indices a numeric vector that provides
#' the new ordering of graph actions. This vector
#' can be the same length as the number of graph
#' actions, or, of shorter length. In the latter
#' case, the ordering places the given items first
#' and the remaining actions will follow.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Add three graph actions to the
#' # graph
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "rescale_node_attrs",
#'     node_attr_from = "pagerank",
#'     node_attr_to = "width",
#'     action_name = "pagerank_to_width") %>%
#'   add_graph_action(
#'     fcn = "set_node_attr_w_fcn",
#'     node_attr_fcn = "get_pagerank",
#'     column_name = "pagerank",
#'     action_name = "get_pagerank") %>%
#'   add_graph_action(
#'     fcn = "colorize_node_attrs",
#'     node_attr_from = "width",
#'     node_attr_to = "fillcolor",
#'     action_name = "pagerank_fillcolor")
#'
#' # View the graph actions for the graph
#' # object by using the `get_graph_actions()`
#' # function
#' graph %>%
#'   get_graph_actions()
#' #> # A tibble: 3 x 3
#' #>   action_index        action_name
#' #>          <dbl>              <chr>
#' #> 1            1  pagerank_to_width
#' #> 2            2       get_pagerank
#' #> 3            3 pagerank_fillcolor
#' #> # ... with 1 more variables:
#' #> #   expression <chr>
#'
#' # We note that the order isn't
#' # correct and that the `get_pagerank`
#' # action should be the 1st action
#' # and `pagerank_to_width` should go
#' # in 2nd place; to fix this, use the
#' # function `reorder_graph_actions()`
#' # and specify the reordering with a
#' # numeric vector
#' graph <-
#'   graph %>%
#'   reorder_graph_actions(
#'     indices = c(2, 1, 3))
#'
#' # View the graph actions for the graph
#' # object once again to verify that
#' # we have the desired order of actions
#' graph %>%
#'   get_graph_actions()
#' #> # A tibble: 3 x 3
#' #>   action_index        action_name
#' #>          <int>              <chr>
#' #> 1            1       get_pagerank
#' #> 2            2  pagerank_to_width
#' #> 3            3 pagerank_fillcolor
#' #> # ... with 1 more variables:
#' #> #   expression <chr>
#' @importFrom dplyr mutate row_number pull
#' @export reorder_graph_actions

reorder_graph_actions <- function(graph,
                                  indices) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Determine whether there any
  # available graph actions
  if (nrow(graph$graph_actions) == 0) {
    stop("There are no graph actions to reorder.")
  }

  # Get the `action_index` values
  # available in `graph$graph_actions`
  available_indices <-
    graph %>%
    get_graph_actions() %>%
    dplyr::pull(action_index)

  # Verify that the provided values
  # do not refer to an `action_index`
  # that does not exist
  if (!any(indices %in% available_indices)) {
    stop("One or more provided indices do not exist in the graph.")
  }

  remaining_indices <-
    which(!(available_indices %in% indices))

  revised_indices <-
    c(indices, remaining_indices)

  # Get a revised data frame with graph actions
  # in the requested order
  revised_graph_actions <-
    graph %>%
    get_graph_actions() %>%
    .[revised_indices, ] %>%
    dplyr::mutate(action_index = row_number())

  # Replace `graph$graph_actions` with the
  # revised version
  graph$graph_actions <- revised_graph_actions

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "reorder_graph_actions",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
