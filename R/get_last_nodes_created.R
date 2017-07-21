#' Get the last set of nodes created in a graph
#' @description Get the last nodes that were created
#' in a graph object of class \code{dgr_graph}.
#' Provides a vector of node ID values. This function
#' should ideally be used just after creating the
#' nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector of node ID values.
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
#' # Get the last nodes created (2 nodes
#' # from the last function call)
#' graph %>%
#'   get_last_nodes_created()
#' #> [1] 3 4
#' @importFrom dplyr mutate filter select pull if_else
#' @importFrom utils tail
#' @export get_last_nodes_created

get_last_nodes_created <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no nodes can be selected.")
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
        tail(1) %>%
        dplyr::pull(step_deleted_nodes) == 1) {
      stop("The previous graph transformation function resulted in a removal of nodes.")
    } else {
      if (nrow(graph_transform_steps) > 1) {
        number_of_nodes_created <-
          (graph_transform_steps %>%
             dplyr::select(nodes) %>%
             tail(2) %>%
             dplyr::pull(nodes))[2] -
          (graph_transform_steps %>%
             dplyr::select(nodes) %>%
             tail(2) %>%
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
      tail(number_of_nodes_created) %>%
      dplyr::pull(id)
  } else {
    node_id_values <- NA
  }

  node_id_values
}
