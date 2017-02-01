#' Select the last edges created in a graph
#' @description Select the last edges that were created
#' in a graph object of class \code{dgr_graph}. This
#' function should ideally be used just after creating
#' the edges to be selected.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph and add a cycle and then
#' # a tree
#' # in 2 separate function calls
#' graph <-
#'   create_graph() %>%
#'     add_cycle(3, rel = "a") %>%
#'     add_balanced_tree(2, 2, rel = "b")
#'
#' # Select the last edges created (all edges
#' # from the tree) and then set their edge
#' # color to be `red`
#' graph <-
#'   graph %>%
#'   select_last_edges_created() %>%
#'   set_edge_attrs_ws("color", "red") %>%
#'   clear_selection()
#'
#' # Display the graph's internal edge
#' # data frame to verify the change
#' get_edge_df(graph)
#' #>   id from to rel color
#' #> 1  1    1  2   a  <NA>
#' #> 2  2    2  3   a  <NA>
#' #> 3  3    3  1   a  <NA>
#' #> 4  4    4  5   b   red
#' #> 5  5    4  6   b   red
#' #> 6  6    5  7   b   red
#' #> 7  7    5  8   b   red
#' #> 8  8    6  9   b   red
#' #> 9  9    6 10   b   red
#' @importFrom dplyr select
#' @importFrom magrittr is_in
#' @importFrom utils tail
#' @export select_last_edges_created

select_last_edges_created <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edges can be selected.")
  }

  # Create bindings for specific variables
  id <- nodes <- NULL

  # Is the last function used on the graph
  # an 'addition of nodes' function?
  if (
    graph$graph_log %>% select(function_used) %>%
    tail(1) %>% .$function_used %>%
    magrittr::is_in(
      c("add_edge", "add_edges_w_string", "add_edge_df",
        "add_forward_edges_ws", "add_reverse_edges_ws",
        "add_edges_from_table", "add_full_graph",
        "add_balanced_tree", "add_cycle",
        "add_path", "add_prism", "add_star")) == FALSE) {
    stop("The previous graph transformation function did not add edges to the graph.")
  }

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

  # Apply the selection of nodes to the graph
  graph <-
    select_edges_by_edge_id(
      graph = graph,
      edges = edge_id_values)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_last_edges_created",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
