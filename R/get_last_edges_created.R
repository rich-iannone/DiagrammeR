#' Get the last set of edges created in a graph
#' @description Get the last edges that were created
#' in a graph object of class \code{dgr_graph}. This
#' function should ideally be used just after creating
#' the edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a vector of edge ID values.
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
#' graph %>%
#'   get_last_edges_created()
#' #> [1] 4 5 6 7 8 9
#' @importFrom dplyr select
#' @importFrom magrittr is_in
#' @importFrom utils tail
#' @export get_last_edges_created

get_last_edges_created <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edges can be selected.")
  }

  # Create bindings for specific variables
  id <- nodes <- edges <- function_used <- NULL

  # Is the last function used on the graph
  # an 'addition of edges' function?
  if (
    graph$graph_log %>%
    dplyr::select(function_used) %>%
    tail(1) %>%
    .$function_used %>%
    magrittr::is_in(
      c("add_edge", "add_edge_clone", "add_edges_w_string",
        "add_edge_df", "add_forward_edges_ws",
        "add_reverse_edges_ws",
        "add_edges_from_table", "add_full_graph",
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
     dplyr::select(edges) %>%
     tail(1) %>%
     .$edges > 0) == FALSE
  )
  {
    stop("The previous graph transformation function did not add edges to the graph.")
  }

  if (
    graph$graph_log %>%
    dplyr::select(function_used) %>%
    tail(1) %>%
    .$function_used %>%
    magrittr::is_in(
      c("add_edge", "add_edges_w_string", "add_edge_df",
        "add_forward_edges_ws", "add_reverse_edges_ws",
        "add_edges_from_table", "add_full_graph",
        "add_balanced_tree", "add_cycle",
        "add_path", "add_prism", "add_star")) == TRUE) {

    # Get the difference in edges between the
    # most recent function and the one previous
    last <-
      graph$graph_log %>%
      dplyr::select(edges) %>%
      tail(2) %>% .$edges %>% .[2]

    second_last <-
      graph$graph_log %>%
      dplyr::select(edges) %>%
      tail(2) %>% .$edges %>% .[1]

    difference_edges <- last - second_last

    # Get ID values for last n edges created
    edge_id_values <-
      graph$edges_df %>%
      dplyr::select(id) %>%
      tail(difference_edges) %>%
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
             dplyr::select(edges) %>%
             tail(1) %>%
             .$edges > 0) {

    edge_id_values <-
      graph$edges_df %>%
      dplyr::select(id) %>%
      .$id
  }

  return(edge_id_values)
}
