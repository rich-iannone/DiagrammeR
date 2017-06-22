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
#' @importFrom dplyr select
#' @importFrom magrittr is_in
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

  # Create bindings for specific variables
  id <- nodes <- function_used <- NULL

  # Is the last function used on the graph
  # an 'addition of nodes' function?
  if (
    graph$graph_log %>%
    dplyr::select(function_used) %>%
    tail(1) %>%
    .$function_used %>%
    magrittr::is_in(
      c("add_node", "add_n_nodes", "add_n_node_clones",
        "add_n_nodes_ws", "add_node_df",
        "add_nodes_from_df_cols",
        "add_nodes_from_table", "add_full_graph",
        "add_balanced_tree", "add_cycle",
        "add_path", "add_prism", "add_star")) == FALSE &
    (graph$graph_log %>%
     dplyr::select(function_used) %>%
     tail(1) %>%
     .$function_used %>%
     magrittr::is_in(
       c("create_graph", "create_random_graph",
         "from_igraph", "from_adj_matrix",
         "import_graph")) &
     graph$graph_log %>%
     dplyr::select(nodes) %>%
     tail(1) %>%
     .$nodes > 0) == FALSE
  )
  {
    stop("The previous graph transformation function did not add nodes to the graph.")
  }

  if (
    graph$graph_log %>%
    dplyr::select(function_used) %>%
    tail(1) %>%
    .$function_used %>%
    magrittr::is_in(
      c("add_node", "add_n_nodes", "add_n_nodes_ws",
        "add_node_df", "add_nodes_from_df_cols",
        "add_nodes_from_table", "add_full_graph",
        "add_balanced_tree", "add_cycle",
        "add_path", "add_prism", "add_star")) == TRUE) {

    # Get the difference in nodes between the
    # most recent function and the one previous
    last <-
      graph$graph_log %>%
      dplyr::select(nodes) %>%
      tail(2) %>% .$nodes %>% .[2]

    second_last <-
      graph$graph_log %>%
      dplyr::select(nodes) %>%
      tail(2) %>% .$nodes %>% .[1]

    difference_nodes <- last - second_last

    # Get ID values for last n nodes created
    node_id_values <-
      graph$nodes_df %>%
      dplyr::select(id) %>%
      tail(difference_nodes) %>%
      .$id

  } else if (graph$graph_log %>%
             dplyr::select(function_used) %>%
             tail(1) %>%
             .$function_used %>%
             magrittr::is_in(
               c("create_graph", "create_random_graph",
                 "from_igraph", "from_adj_matrix",
                 "import_graph")) &
             graph$graph_log %>%
             dplyr::select(nodes) %>%
             tail(1) %>%
             .$nodes > 0) {

    node_id_values <-
      graph$nodes_df %>%
      dplyr::select(id) %>%
      .$id
  }

  return(node_id_values)
}
