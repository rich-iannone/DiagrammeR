#' Traverse out node-by_node until stopping conditions are met
#' @description From a graph object of
#' class \code{dgr_graph}, move along
#' outward edges from one or more nodes
#' present in a selection to other
#' connected nodes, replacing the current
#' nodes in the selection with those nodes
#' traversed to until reaching nodes that
#' satisfy one or more conditions.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param conditions an option to use a
#' stopping condition for the traversal.
#' If the condition is met during the
#' traversal (i.e., the node(s) traversed
#' to match the condition), then those
#' traversals will terminate at those
#' nodes. Otherwise, traversals with
#' continue and terminate when the number
#' of steps provided in \code{max_steps}
#' is reached.
#' @param max_steps the maximum number
#' of \code{trav_out()} steps (i.e.,
#' node-to-node traversals in the outward
#' direction) to allow before stopping.
#' @param keep_caught_only if \code{TRUE}
#' then we should exclude any nodes not
#' satisfying the conditions provided in
#' \code{conditions} from the final node
#' selection.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a path graph and add
#' # values of 1 to 10 across the
#' # nodes from beginning to end;
#' # select the first path node
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 10,
#'     node_data = node_data(
#'       value = 1:10)) %>%
#'   select_nodes_by_id(
#'     nodes = 1)
#'
#' # Traverse outward, node-by-node
#' # until stopping at a node where
#' # the `value` attribute is 8
#' graph <-
#'   graph %>%
#'   trav_out_until(
#'     conditions =
#'       value == 8)
#'
#' # Get the graph's node selection
#' graph %>%
#'   get_selection()
#' #> [1] 8
#'
#' # Create two cycles in graph and
#' # add values of 1 to 6 to the
#' # first cycle, and values 7 to
#' # 12 in the second; select nodes
#' # `1` and `7`
#' graph <-
#'   create_graph() %>%
#'   add_cycle(
#'     n = 6,
#'     node_data = node_data(
#'       value = 1:6)) %>%
#'   add_cycle(
#'     n = 6,
#'     node_data = node_data(
#'       value = 7:12)) %>%
#'   select_nodes_by_id(
#'     nodes = c(1, 7))
#'
#' # Traverse outward, node-by-node
#' # from `1` and `7` until stopping
#' # at the first nodes where the
#' # `value` attribute is 5, 6, or 15;
#' # specify that we should only
#' # keep the finally traversed to
#' # nodes that satisfy the conditions
#' graph <-
#'   graph %>%
#'   trav_out_until(
#'     conditions =
#'       value %in% c(5, 6, 15),
#'     keep_caught_only = TRUE)
#'
#' # Get the graph's node selection
#' graph %>%
#'   get_selection()
#' #> [1] 5
#' @importFrom rlang enquo UQ
#' @export trav_out_until

trav_out_until <- function(graph,
                           conditions,
                           max_steps = 30,
                           keep_caught_only = FALSE) {

  conditions <- rlang::enquo(conditions)

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no traversal can occur.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no traversal can occur.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes, so, no traversal can occur.")
  }

  # Initialize the node stack and
  # the step count
  node_stack <- vector(mode = "integer")
  step <- 1

  # Determine which nodes satisfy the
  # conditions provided
  all_nodes_conditions_met <-
    get_node_ids(x = graph, conditions = rlang::UQ(conditions))

  if (keep_caught_only & all(is.na(all_nodes_conditions_met))) {

    # Clear the active selection
    graph <-
      graph %>% clear_selection()

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "trav_out_until",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

    # Perform graph actions, if any are available
    if (nrow(graph$graph_actions) > 0) {
      graph <-
        graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (graph$graph_info$write_backups) {
      save_graph_as_rds(graph = graph)
    }

    return(graph)
  }

  repeat {

    # Perform traversal
    graph <- graph %>% trav_out()

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]

    # If any nodes are `all_nodes_conditions_met` nodes
    # deselect that node and save the node in a stack
    if (any(graph %>% get_selection() %in% all_nodes_conditions_met)) {

      node_stack <-
        c(node_stack,
          intersect(
            graph %>% get_selection(),
            all_nodes_conditions_met))

      # Remove the node from the active selection
      graph <-
        graph %>% deselect_nodes(nodes = node_stack)

      # Remove action from graph log
      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]
    }

    if (all(is.na(get_selection(graph)))) break

    step <- step + 1

    if (step == max_steps) break
  }

  if (length(node_stack > 0)) {

    graph <-
      graph %>%
      select_nodes_by_id(unique(node_stack))

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]
  }

  if (keep_caught_only & !all(is.na(get_selection(graph)))) {

    new_selection <- get_selection(graph)

    graph <-
      graph %>%
      clear_selection() %>%
      select_nodes_by_id(
        intersect(new_selection, all_nodes_conditions_met))

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "trav_out_until",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
