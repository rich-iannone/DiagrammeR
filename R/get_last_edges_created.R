#' Get the last set of edges created in a graph
#'
#' Get the last edges that were created in a graph object of class `dgr_graph`.
#' This function should ideally be used just after creating the edges.
#'
#' @inheritParams render_graph
#'
#' @return A vector of edge ID values.
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
#' # Get the last edges created (all edges
#' # from the tree)
#' graph %>% get_last_edges_created()
#'
#' @export
get_last_edges_created <- function(graph) {

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

  edge_id_values
}
