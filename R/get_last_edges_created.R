#' Get the last set of edges created in a graph
#'
#' @description
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

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  graph_transform_steps <-
    graph$graph_log %>%
    dplyr::mutate(
      step_created_edges = as.integer(function_used %in% edge_creation_functions()),
      step_deleted_edges = as.integer(function_used %in% edge_deletion_functions()),
      step_init_with_edges = as.integer(function_used %in% graph_init_functions() &
                                          edges > 0)
    ) %>%
    dplyr::filter(
      dplyr::if_any(
        .cols = c(step_created_edges, step_deleted_edges, step_init_with_edges),
        .fns = function(x) x == 1
        )
    ) %>%
    dplyr::select(-"version_id", -"time_modified", -"duration")

  if (nrow(graph_transform_steps) > 0) {

    if (graph_transform_steps %>%
        utils::tail(1) %>%
        dplyr::pull(step_deleted_edges) == 1) {

      abort("The previous graph transformation function resulted in a removal of edges.")

    } else {
      if (nrow(graph_transform_steps) > 1) {
        number_of_edges_created <-
          (graph_transform_steps %>%
             dplyr::select("edges") %>%
             utils::tail(2) %>%
             dplyr::pull("edges"))[2] -
          (graph_transform_steps %>%
             dplyr::select("edges") %>%
             utils::tail(2) %>%
             dplyr::pull("edges"))[1]
      } else {
        number_of_edges_created <-
          graph_transform_steps %>%
          dplyr::pull("edges")
      }
    }

    edge_id_values <-
      graph$edges_df %>%
      dplyr::select("id") %>%
      utils::tail(number_of_edges_created) %>%
      dplyr::pull("id")
  } else {
    edge_id_values <- NA
  }

  edge_id_values
}
