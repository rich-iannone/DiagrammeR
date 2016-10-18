#' Add an edge between nodes in a graph object
#' @description With a graph object of class
#' \code{dgr_graph}, add an edge to nodes within the
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param from the outgoing node from which the edge
#' is connected.
#' @param to the incoming nodes to which each edge
#' is connected.
#' @param rel an optional string specifying the
#' relationship between the
#' connected nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with two nodes
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(2)
#'
#' # Add an edge between those nodes and attach a
#' # relationship to the edge
#' graph <-
#'  add_edge(
#'    graph,
#'    from = 1,
#'    to = 2,
#'    rel = "to_get")
#'
#' # Use the `edge_info()` function to verify that
#' # the edge has been created
#' edge_info(graph)
#' #>   from to    rel
#' #> 1    1  2 to_get
#'
#' # Add another node and edge to the graph
#' graph <-
#'   graph %>%
#'   add_node %>%
#'   add_edge(3, 2, "to_get")
#'
#' # Verify that the edge has been created by
#' # getting a count of graph edges
#' edge_count(graph)
#' #> [1] 2
#' @export add_edge

add_edge <- function(graph,
                     from,
                     to,
                     rel = NULL) {

  if (is_graph_empty(graph)) {
    stop("Edges cannot be added to an empty graph.")
  }

  if (length(from) > 1 | length(to) > 1) {
    stop("Only one edge can be specified.")
  }

  # If an edge between nodes is requested and that
  # edge exists, stop function
  if (all(
    !is.na(get_edges(graph,
                     return_type = "vector")))) {
    if (any(
      get_edges(
        graph, return_type = "list")[[1]] == from &
      get_edges(
        graph, return_type = "list")[[2]] == to)) {
      stop("This edge already exists.")
    }
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # If `graph$edges_df` is NULL then use
  # `create_edges()` to add an edge
  if (is.null(graph$edges_df)) {

    # If a relationship is defined, add that in
    # the `create_edges()` call
    if (!is.null(rel)) {

      edf <-
        create_edge_df(
          from = from,
          to = to,
          rel = rel)
    }

    # If a relationship is not defined, use a simpler
    # `create_edges()` call
    if (is.null(rel)) {

      edf <-
        create_edge_df(
          from = from,
          to = to,
          rel = rel)
    }

    # Add the edge data frame to the graph
    graph$edges_df <- edf

    # Update the `last_node` counter
    graph$last_node <- nodes_created

    return(graph)
  }

  # If `graph$edges_df` is not NULL then use both
  # `combine_edges()` and `create_edge_df()` to
  # add an edge
  if (!is.null(graph$edges_df)) {

    # If a relationship is defined, add that in the
    # `create_edges()` call
    if (!is.null(rel)) {

      combined_edges <-
        combine_edges(
          graph$edges_df,
          create_edge_df(
            from = from,
            to = to,
            rel = rel))
    }

    # If a relationship is not defined, use a simpler
    # `create_edges()` call
    if (is.null(rel)) {

      combined_edges <-
        combine_edges(
          graph$edges_df,
          create_edge_df(
            from = from,
            to = to))
    }

    # Ensure that the `from` and `to` columns are
    # classed as `integer`
    combined_edges[, 1] <-
      as.integer(combined_edges[, 1])

    combined_edges[, 2] <-
      as.integer(combined_edges[, 2])

    # Use the `combined_edges` object as a
    # replacement for the graph's internal
    # edge data frame

    # Add the edge data frame to the graph
    graph$edges_df <- combined_edges

    # Update the `last_node` counter
    graph$last_node <- nodes_created

    return(graph)
  }
}
