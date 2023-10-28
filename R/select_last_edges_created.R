#' Select the last set of edges created in a graph
#'
#' @description
#'
#' Select the last edges that were created in a graph object of class
#' `dgr_graph`. This function should ideally be used just after creating the
#' edges to be selected.
#'
#' @inheritParams render_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph and add a cycle and then
#' # a tree in 2 separate function calls
#' graph <-
#'   create_graph() %>%
#'   add_cycle(
#'     n = 3,
#'     rel = "a") %>%
#'   add_balanced_tree(
#'     k = 2, h = 2,
#'     rel = "b")
#'
#' # Select the last edges created (all edges
#' # from the tree) and then set their edge
#' # color to be `red`
#' graph <-
#'   graph %>%
#'   select_last_edges_created() %>%
#'   set_edge_attrs_ws(
#'     edge_attr = color,
#'     value = "red") %>%
#'   clear_selection()
#'
#' # Display the graph's internal edge
#' # data frame to verify the change
#' graph %>% get_edge_df()
#'
#' @export
select_last_edges_created <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  graph_transform_steps <-
    graph$graph_log %>%
    dplyr::mutate(step_created_edges = dplyr::if_else(
      function_used %in% edge_creation_functions(), 1, 0)) %>%
    dplyr::mutate(step_deleted_edges = dplyr::if_else(
      function_used %in% edge_deletion_functions(), 1, 0)) %>%
    dplyr::mutate(step_init_with_edges = dplyr::if_else(
      function_used %in% graph_init_functions() &
        edges > 0, 1, 0)) %>%
    dplyr::filter(
      step_created_edges == 1 | step_deleted_edges == 1 | step_init_with_edges) %>%
    dplyr::select(-version_id, -time_modified, -duration)

  if (nrow(graph_transform_steps) > 0) {

    if (graph_transform_steps %>%
        utils::tail(1) %>%
        dplyr::pull(step_deleted_edges) == 1) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The previous graph transformation function resulted in a removal of edges")

    } else {
      if (nrow(graph_transform_steps) > 1) {
        number_of_edges_created <-
          (graph_transform_steps %>%
             dplyr::select(edges) %>%
             utils::tail(2) %>%
             dplyr::pull(edges))[2] -
          (graph_transform_steps %>%
             dplyr::select(edges) %>%
             utils::tail(2) %>%
             dplyr::pull(edges))[1]
      } else {
        number_of_edges_created <-
          graph_transform_steps %>%
          dplyr::pull(edges)
      }
    }

    edge_id_values <-
      graph$edges_df %>%
      dplyr::select(id) %>%
      utils::tail(number_of_edges_created) %>%
      dplyr::pull(id)
  } else {
    edge_id_values <- NA
  }

  if (!any(is.na(edge_id_values))) {

    # Apply the selection of edges to the graph
    graph <-
      suppressMessages(
        select_edges_by_edge_id(
          graph = graph,
          edges = edge_id_values)
      )

    # Update the `graph_log` df with an action
    graph$graph_log <-
      graph$graph_log[-nrow(graph$graph_log),] %>%
      add_action_to_log(
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
  }

  graph
}
