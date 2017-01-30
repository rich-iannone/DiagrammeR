#' Select the last nodes created in a graph
#' @description Select the last nodes that were created
#' in a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph and add 4 nodes
#' # in 2 separate function calls
#' graph <-
#'   create_graph() %>%
#'     add_n_nodes(
#'       2, "a", label = c("a_1", "a_2")) %>%
#'     add_n_nodes(
#'       2, "b", label = c("b_1", "b_2"))
#'
#' # Select the last nodes created
#' # (2 nodes from the last function call)
#' graph <-
#'   graph %>%
#'   select_last_nodes_created() %>%
#'   set_node_attrs_ws("color", "red") %>%
#'   clear_selection()
#'
#' # Display the graph's internal node
#' # data frame to verify the change
#' get_node_df(graph)
#' #>   id type label color
#' #> 1  1    a   a_1  <NA>
#' #> 2  2    a   a_2  <NA>
#' #> 3  3    b   b_1   red
#' #> 4  4    b   b_2   red
#' @importFrom dplyr select
#' @importFrom magrittr is_in
#' @export select_last_nodes_created

select_last_nodes_created <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no node can be selected.")
  }

  # Create bindings for specific variables
  id <- nodes <- NULL

  # Is the last function used on the graph
  # an 'addition of nodes' function?
  if (
    graph$graph_log %>% select(function_used) %>%
    tail(1) %>% .$function_used %>%
    magrittr::is_in(
      c("add_node", "add_n_nodes", "add_n_nodes_ws", "add_node_df",
        "add_nodes_from_df_cols", "add_nodes_from_table",
        "add_full_graph", "add_balanced_tree", "add_cycle",
        "add_path", "add_prism", "add_star")) == FALSE) {
    stop("The previous graph transformation function did not add nodes to the graph.")
  }

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

  # Apply the selection of nodes to the graph
  graph <-
    select_nodes(
      graph = graph,
      nodes = node_id_values)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_last_nodes_created",
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
