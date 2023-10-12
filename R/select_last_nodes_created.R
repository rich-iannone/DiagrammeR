#' Select the last set of nodes created in a graph
#'
#' @description
#'
#' Select the last nodes that were created in a graph object of class
#' `dgr_graph`. This function should ideally be used just after creating the
#' nodes to be selected.
#'
#' @inheritParams render_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create a graph and add 4 nodes
#' # in 2 separate function calls
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(
#'     n = 2,
#'     type = "a",
#'     label = c("a_1", "a_2")) %>%
#'   add_n_nodes(
#'     n = 2,
#'     type = "b",
#'     label = c("b_1", "b_2"))
#'
#' # Select the last nodes created (2 nodes
#' # from the last function call) and then
#' # set their color to be `red`
#' graph <-
#'   graph %>%
#'   select_last_nodes_created() %>%
#'   set_node_attrs_ws(
#'     node_attr = color,
#'     value = "red") %>%
#'   clear_selection()
#'
#' # Display the graph's internal node
#' # data frame to verify the change
#' graph %>% get_node_df()
#'
#' @export
select_last_nodes_created <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes.")
  }

  graph_transform_steps <-
    graph$graph_log %>%
    dplyr::mutate(step_created_nodes = dplyr::if_else(
      function_used %in% node_creation_functions(), 1, 0)) %>%
    dplyr::mutate(step_deleted_nodes = dplyr::if_else(
      function_used %in% node_deletion_functions(), 1, 0)) %>%
    dplyr::mutate(step_init_with_nodes = dplyr::if_else(
      function_used %in% graph_init_functions() &
        nodes > 0, 1, 0)) %>%
    dplyr::filter(
      step_created_nodes == 1 | step_deleted_nodes == 1 | step_init_with_nodes) %>%
    dplyr::select(-version_id, -time_modified, -duration)

  if (nrow(graph_transform_steps) > 0) {

    if (graph_transform_steps %>%
        utils::tail(1) %>%
        dplyr::pull(step_deleted_nodes) == 1) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The previous graph transformation function resulted in a removal of nodes")

    } else {
      if (nrow(graph_transform_steps) > 1) {
        number_of_nodes_created <-
          (graph_transform_steps %>%
             dplyr::select(nodes) %>%
             utils::tail(2) %>%
             dplyr::pull(nodes))[2] -
          (graph_transform_steps %>%
             dplyr::select(nodes) %>%
             utils::tail(2) %>%
             dplyr::pull(nodes))[1]
      } else {
        number_of_nodes_created <-
          graph_transform_steps %>%
          dplyr::pull(nodes)
      }
    }

    node_id_values <-
      graph$nodes_df %>%
      dplyr::select(id) %>%
      utils::tail(number_of_nodes_created) %>%
      dplyr::pull(id)
  } else {
    node_id_values <- NA
  }

  if (!any(is.na(node_id_values))) {

    # Apply the selection of nodes to the graph
    graph <-
      suppressMessages(
        select_nodes(
          graph = graph,
          nodes = node_id_values))

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
