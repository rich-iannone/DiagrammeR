#' Get the graph's ndf filtered by a selection of nodes
#'
#' From a graph object of class \code{dgr_graph}, get the graph's internal node
#' data frame that is filtered by the node ID values currently active as a
#' selection.
#'
#' This function makes use of an active selection of nodes (and the
#' function ending with \code{_ws} hints at this).
#'
#' Selections of nodes can be performed using the following node selection
#' (\code{select_*()}) functions:
#' \code{\link{select_nodes}()},
#' \code{\link{select_last_nodes_created}()},
#' \code{\link{select_nodes_by_degree}()},
#' \code{\link{select_nodes_by_id}()}, or
#' \code{\link{select_nodes_in_neighborhood}()}.
#'
#' Selections of nodes can also be performed using the following traversal
#' (\code{trav_*()}) functions:
#' \code{\link{trav_out}()},
#' \code{\link{trav_in}()},
#' \code{\link{trav_both}()},
#' \code{\link{trav_out_node}()},
#' \code{\link{trav_in_node}()},
#' \code{\link{trav_out_until}()}, or
#' \code{\link{trav_in_until}()}.
#' @inheritParams render_graph
#' @return a node data frame.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 4,
#'     m = 4,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = c(2.5, 8.2, 4.2, 2.4))
#'
#' # Select nodes with ID values
#' # `1` and `3`
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(
#'     nodes = c(1, 3))
#'
#' # Get the node data frame that's
#' # limited to the rows that correspond
#' # to the node selection
#' graph %>% get_node_df_ws()
#' @importFrom dplyr filter
#' @export
get_node_df_ws <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Validation: Graph object has a valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There is no selection of nodes available.")
  }

  # Extract the node data frame (ndf)
  # from the graph and get only those nodes
  # from the node selection
  graph$nodes_df %>%
    dplyr::filter(id %in% graph$node_selection$node)
}
