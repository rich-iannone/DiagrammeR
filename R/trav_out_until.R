#' Traverse outward node-by-node until stopping conditions are met
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, move along outward edges from one
#' or more nodes present in a selection to other connected nodes, replacing the
#' current nodes in the selection with those nodes traversed to until reaching
#' nodes that satisfy one or more conditions.
#'
#' This traversal function makes use of an active selection of nodes. After the
#' traversal, depending on the traversal conditions, there will either be a
#' selection of nodes or no selection at all.
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
#' @param conditions An option to use a stopping condition for the traversal. If
#'   the condition is met during the traversal (i.e., the node(s) traversed to
#'   match the condition), then those traversals will terminate at those nodes.
#'   Otherwise, traversals with continue and terminate when the number of steps
#'   provided in `max_steps` is reached.
#' @param max_steps The maximum number of `trav_out()` steps (i.e., node-to-node
#'   traversals in the outward direction) to allow before stopping.
#' @param exclude_unmatched If `TRUE` (the default value) then any nodes not
#'   satisfying the conditions provided in `conditions` that are in the ending
#'   selection are excluded.
#' @param add_to_selection If `TRUE` then every node traversed will be part of
#'   the final selection of nodes. If `FALSE` (the default value) then only the
#'   nodes finally traversed to will be part of the final node selection.
#'
#' @return A graph object of class `dgr_graph`.
#'
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
#' graph %>% get_selection()
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
#'       value %in% c(5, 6, 9),
#'     exclude_unmatched = TRUE)
#'
#' # Get the graph's node selection
#' graph %>% get_selection()
#'
#' @export
trav_out_until <- function(
    graph,
    conditions,
    max_steps = 30,
    exclude_unmatched = TRUE,
    add_to_selection = FALSE
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)
  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)
  # Validation: Graph contains edges
  check_graph_contains_edges(graph)
  # Validation: Graph object has valid node selection
  check_graph_contains_node_selection(
    graph,
    c("Any traversal requires an active selection.",
      "This type of traversal requires a selection of nodes."))

  # Initialize the node stack and
  # the step count
  node_stack <- vector(mode = "integer")
  step <- 0

  starting_nodes <-
    suppressMessages(
      graph %>%
        get_selection())

  # Determine which nodes satisfy the
  # conditions provided
  all_nodes_conditions_met <-
    graph %>%
    get_node_ids(conditions = {{ conditions }})

  if (exclude_unmatched && all(is.na(all_nodes_conditions_met))) {

    # Clear the active selection
    graph <-
      suppressMessages(
        graph %>%
          clear_selection())

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]

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

    # Perform graph actions, if any are available
    if (nrow(graph$graph_actions) > 0) {
      graph <-
        trigger_graph_actions(graph)
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
    if (any(suppressMessages(graph %>% get_selection()) %in%
            all_nodes_conditions_met)) {

      node_stack <-
        c(node_stack,
          intersect(
            suppressMessages(graph %>% get_selection()),
            all_nodes_conditions_met))

      # Remove the node from the active selection
      graph <-
        graph %>% deselect_nodes(nodes = node_stack)

      # Remove action from graph log
      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]
    }

    if (all(is.na(suppressMessages(get_selection(graph))))) break

    step <- step + 1

    if (step == max_steps) break
  }

  if (length(node_stack > 0)) {

    if (add_to_selection) {

      if (exclude_unmatched) {
        node_stack <-
          intersect(node_stack, all_nodes_conditions_met)
      }

      path_nodes <-
        node_stack %>%
        purrr::map(
          .f = function(x) {
            graph %>%
              to_igraph() %>%
              igraph::all_simple_paths(
                from = x,
                to = starting_nodes,
                mode = "in") %>%
              unlist() %>%
              as.integer()}) %>%
        unlist() %>%
        unique()

      graph <-
        graph %>%
        select_nodes_by_id(unique(path_nodes))

    } else {

      graph <-
        graph %>%
        select_nodes_by_id(unique(node_stack))
    }

    # Remove action from graph log
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log), ]

  } else if (length(node_stack) < 1) {

    if (exclude_unmatched &&
        !all(is.na(suppressMessages(get_selection(graph))))) {

      new_selection <- suppressMessages(get_selection(graph))

      graph <-
        suppressMessages(
          graph %>%
            clear_selection() %>%
            select_nodes_by_id(
              intersect(new_selection, all_nodes_conditions_met)))

      # Remove action from graph log
      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]

      # Remove action from graph log
      graph$graph_log <-
        graph$graph_log[-nrow(graph$graph_log), ]
    }
  }

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

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      trigger_graph_actions(graph)
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
