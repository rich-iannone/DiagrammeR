#' Determine whether a specified edge is present
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, determine whether an edge (defined
#' by a pair of node IDs or node label values) is present.
#'
#' @inheritParams render_graph
#' @param edge An edge ID value to test for presence in the graph. If a single,
#'   numeric value is provided then values for `from` or `to` needn't be
#'   supplied.
#' @param from A node ID from which the edge is outgoing, or, the label
#'   associated with the node. For an undirected graph, the value in `from` can
#'   be interchangeable with that in `to`.
#' @param to A node ID to which the edge is incoming, or, the label associated
#'   with the node. For an undirected graph, the value in `to` can be
#'   interchangeable with that in `from`.
#'
#' @return A logical value.
#'
#' @examples
#' # Create a simple graph with
#' # a path of four nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 4,
#'     type = "path",
#'     label = c("one", "two",
#'               "three", "four"))
#'
#' # Find out if edge ID `3`
#' # is present in the graph
#' graph %>%
#'   is_edge_present(edge = 3)
#'
#' # Determine if there are any edges
#' # with the definition `1` -> `2`
#' graph %>%
#'   is_edge_present(
#'     from = 1,
#'     to = 2)
#'
#' # Determine if there are any edges
#' # with the definition `4` -> `5`
#' graph %>%
#'   is_edge_present(
#'     from = 4,
#'     to = 5)
#'
#' # Determine whether an edge,
#' # defined by its labels as
#' # `two` -> `three`, exists in
#' # the graph
#' graph %>%
#'   is_edge_present(
#'     from = "two",
#'     to = "three")
#'
#' # Set the graph as undirected
#' # and determine whether an
#' # edge between nodes with labels
#' # `three` and `two` exists
#' graph %>%
#'   set_graph_undirected() %>%
#'   is_edge_present(
#'     from = "three",
#'     to = "two")
#'
#' @export
is_edge_present <- function(
    graph,
    edge = NULL,
    from = NULL,
    to = NULL
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  use_edge <- !is.null(edge)

  use_from_to <- !is.null(from) && !is.null(to)

  if (use_edge && use_from_to) {
    use_from_to <- FALSE
  }

  if (!use_edge && !use_from_to) {
    cli::cli_abort(
      "Either provide an edge ID or a pair of nodes to test for edge presence.")
  }

  if (use_edge) {

    # Verify that what is provided for `edge`
    # is a numeric value of length 1
    check_number_decimal(edge)

    edge_is_in_graph <- edge %in% graph$edges_df$id

    return(edge_is_in_graph)
  }

  if (use_from_to) {

    # Verify that each of the values for `from` and
    # `to` are given as a single value
    from_is_single_value <- length(from) == 1
    to_is_single_value   <- length(to) == 1

    # Stop function if either node is not a single value
    if (!from_is_single_value || !to_is_single_value) {
      cli::cli_abort(
        "Only single nodes for `from` and `to` should be specified.")
    }

    if (inherits(from, "character") && inherits(to, "character")) {

      # Determine whether the pair of
      # labels provided are in the graph
      nodes_available_in_graph <-
        all(c(from, to) %in% graph$nodes_df$label)

      # Return FALSE if both nodes not
      # present in graph
      if (!nodes_available_in_graph) {
        return(FALSE)
      }

      # Use the `translate_to_node_id()` helper function to map
      # node `label` values to node `id` values
      from_to_node_id <-
        translate_to_node_id(
          graph = graph,
          from = from,
          to = to)

      from <- from_to_node_id$from
      to <- from_to_node_id$to

      # Determine whether a matching edge is
      # available in the graph
      if (graph$directed) {

        edge_is_in_graph <-
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to)

      } else if (!graph$directed) {

        edge_is_in_graph <-
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to) |
              any(graph$edges_df$from == to &
                    graph$edges_df$to == from)
      }

      return(edge_is_in_graph)
    }

    if (inherits(from, "numeric") && inherits(to, "numeric")) {

      # Determine whether the pair of
      # labels provided are in the graph
      if (from_is_single_value && to_is_single_value) {
        nodes_available_in_graph <-
          all(c(from, to) %in% get_node_ids(graph))
      }

      # Return FALSE if both nodes not
      # present in graph
      if (!nodes_available_in_graph) {
        return(FALSE)
      }

      # Determine whether a matching edge is
      # available in the graph
      if (graph$directed) {

        edge_is_in_graph <-
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to)

      } else if (!graph$directed) {

        edge_is_in_graph <-
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to) |
              any(graph$edges_df$from == to &
                    graph$edges_df$to == from)
      }

      return(edge_is_in_graph)
    }
  }
}
