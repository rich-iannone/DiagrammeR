# Function for checking graph
check_graph_valid <- function(graph, error_call = rlang::caller_env()) {
  if (!graph_object_valid(graph)) {
    rlang::abort(
      "The graph is not valid.",
      call = error_call)
  }
}

check_graph_contains_nodes <- function(graph,
                                       extra_msg = NULL,
                                       error_call = rlang::caller_env()) {
  if (!graph_contains_nodes(graph)) {
    rlang::abort(c(
      "The graph does not contain nodes.",
      extra_msg),
      call = error_call)
  }
}

check_graph_contains_edges <- function(graph, error_call = rlang::caller_env()) {
  if (!graph_contains_edges(graph)) {
    rlang::abort(
      "The graph contains no edges.",
      call = error_call)
  }
}

check_graph_contains_node_selection <- function(graph,
                                                error_call = rlang::caller_env()) {
  # FIXME add there is no selection of nodes available as extra_msg.
  if (!graph_contains_node_selection(graph)) {
    rlang::abort(c(
      "There is no selection of nodes available.",
      "Any traversal requires an active selection.",
      "This type of traversal requires a selection of nodes."
    ), call = error_call)
  }
}

check_graph_contains_edge_selection <- function(graph,
                                                extra_msg = NULL,
                                                error_call = caller_env()) {
  if (!graph_contains_edge_selection(graph)) {
    rlang::abort(c(
      "The graph contains no selection of edges.",
      extra_msg),
      call = error_call
    )
  }
}

check_property_graph <- function(graph,
                                 error_call = caller_env()) {
  if (!is_property_graph(graph)) {
    abort(c(
      "The graph object is not a property graph"),
      call = error_call
    )
  }
}
