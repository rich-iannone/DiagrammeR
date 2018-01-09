#' Determine whether a specified edge is present
#' @description From a graph object of class
#' \code{dgr_graph}, determine whether an edge
#' (defined by a pair of node IDs or node label
#' values) is present.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge an edge ID value to test for
#' presence in the graph. If a single, numeric
#' value is provided then values for \code{from}
#' or \code{to} needn't be supplied.
#' @param from a node ID from which the edge
#' is outgoing, or, the label associated with
#' the node. For an undirected graph, the
#' value in \code{from} can be interchangeable
#' with that in \code{to}.
#' @param to a node ID to which the edge is
#' incoming, or, the label associated with
#' the node. For an undirected graph, the
#' value in \code{to} can be interchangeable
#' with that in \code{from}.
#' @return a logical value.
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
#' is_edge_present(
#'   graph = graph,
#'   edge = 3)
#' #> [1] TRUE
#'
#' # Determine if there are any edges
#' # with the definition `1` -> `2`
#' is_edge_present(
#'   graph = graph,
#'   from = 1,
#'   to = 2)
#' #> [1] TRUE
#'
#' # Determine if there are any edges
#' # with the definition `4` -> `5`
#' is_edge_present(
#'   graph = graph,
#'   from = 4,
#'   to = 5)
#' #> [1] FALSE
#'
#' # Determine whether an edge,
#' # defined by its labels as
#' # `two` -> `three`, exists in
#' # the graph
#' is_edge_present(
#'   graph = graph,
#'   from = "two",
#'   to = "three")
#' #> [1] TRUE
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
#' #> [1] TRUE
#' @export is_edge_present

is_edge_present <- function(graph,
                            edge = NULL,
                            from = NULL,
                            to = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  if (!is.null(edge)) {
    use_edge <- TRUE
  } else {
    use_edge <- FALSE
  }

  if (!is.null(from) & !is.null(to)) {
    use_from_to <- TRUE
  } else {
    use_from_to <- FALSE
  }

  if (use_edge & use_from_to) {
    use_from_to <- FALSE
  }

  if (use_edge == FALSE & use_from_to == FALSE) {

    stop(
      "Either provide an edge ID or a pair of nodes to test for edge presence.",
      call. = FALSE)
  }

  if (use_edge) {

    # Verify that what is provided for `edge`
    # is a numeric value of length 1
    if (!inherits(edge, "numeric") | length(edge) != 1) {

      stop(
        "For `edge`, a single, numeric value must be provided.",
        call. = FALSE)
    }

    edge_is_in_graph <-
      ifelse(edge %in% graph$edges_df$id, TRUE, FALSE)

    return(edge_is_in_graph)
  }

  if (use_from_to) {

    # Verify that each of the values for `from` and
    # `to` are given as a single value
    from_is_single_value <-
      ifelse(length(from) == 1, TRUE, FALSE)

    to_is_single_value <-
      ifelse(length(to) == 1, TRUE, FALSE)

    # Stop function if either node is not a single value
    if (from_is_single_value == FALSE |
        to_is_single_value == FALSE) {

      stop(
        "Only single nodes for `from` and `to` should be specified.",
        call. = FALSE)
    }

    if (inherits(from, "character") & inherits(to, "character")) {

      # Determine whether the pair of
      # labels provided are in the graph
      nodes_available_in_graph <-
        ifelse(all(c(from, to) %in%
                     graph$nodes_df$label), TRUE, FALSE)

      # Return FALSE if both nodes not
      # present in graph
      if (nodes_available_in_graph == FALSE) {
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
          ifelse(
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to),
            TRUE, FALSE)

      } else if (graph$directed == FALSE) {

        edge_is_in_graph <-
          ifelse(
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to) |
              any(graph$edges_df$from == to &
                    graph$edges_df$to == from),
            TRUE, FALSE)
      }

      return(edge_is_in_graph)
    }

    if (inherits(from, "numeric") & inherits(to, "numeric")) {

      # Determine whether the pair of
      # labels provided are in the graph
      if (from_is_single_value & to_is_single_value) {
        nodes_available_in_graph <-
          ifelse(all(c(from, to) %in%
                       get_node_ids(graph)), TRUE, FALSE)
      }

      # Return FALSE if both nodes not
      # present in graph
      if (nodes_available_in_graph == FALSE) {
        return(FALSE)
      }

      # Determine whether a matching edge is
      # available in the graph
      if (graph$directed) {

        edge_is_in_graph <-
          ifelse(
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to),
            TRUE, FALSE)

      } else if (graph$directed == FALSE) {

        edge_is_in_graph <-
          ifelse(
            any(graph$edges_df$from == from &
                  graph$edges_df$to == to) |
              any(graph$edges_df$from == to &
                    graph$edges_df$to == from),
            TRUE, FALSE)
      }

      return(edge_is_in_graph)
    }
  }
}
