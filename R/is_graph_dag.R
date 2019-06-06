#' Is the graph a directed acyclic graph?
#'
#' Provides a logical value on whether the graph is a directed acyclic graph
#' (DAG). The conditions for a graph that is a DAG are that it should be a
#' directed graph and it should not contain any cycles.
#'
#' @inheritParams render_graph
#' @return A logical value.
#' @examples
#' # Create a directed graph containing
#' # only a balanced tree
#' graph_tree <-
#'   create_graph() %>%
#'   add_balanced_tree(
#'     k = 2, h = 3)
#'
#' # Determine whether this graph
#' # is a DAG
#' graph_tree %>%
#'   is_graph_dag()
#'
#' # Create a directed graph containing
#' # a single cycle
#' graph_cycle <-
#'   create_graph() %>%
#'   add_cycle(n = 5)
#'
#' # Determine whether this graph
#' # is a DAG
#' graph_cycle %>%
#'   is_graph_dag()
#'
#' # Create an undirected graph
#' # containing a balanced tree
#' graph_tree_undirected <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_balanced_tree(
#'     k = 2, h = 2)
#'
#' # Determine whether this graph
#' # is a DAG
#' graph_tree_undirected %>%
#'   is_graph_dag()
#'
#' @export
is_graph_dag <- function(graph) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # If the graph contains no nodes, it
  # cannot be a DAG
  if (nrow(graph$nodes_df) == 0) {
    return(FALSE)
  }

  # Convert the graph to an igraph object
  ig_graph <- to_igraph(graph)

  # Determine whether the graph is
  # a simple graph
  igraph::is_dag(ig_graph)
}
