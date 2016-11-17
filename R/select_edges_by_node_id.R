#' Select edges in a graph using node ID values
#' @description Select edges in a graph object of class
#' \code{dgr_graph} using node ID values. All edges
#' associated with the provided nodes will be included
#' in the selection. If nodes have IDs that are
#' monotonically increasing integer values, then
#' numeric ranges can be provided for the selection.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param nodes a vector of node IDs for the selection
#' of nodes present in the graph.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a tree structure that's
#' # 3 levels deep (begins with node `1`, branching
#' # by 3 nodes at each level); the resulting graph
#' # contains 13 nodes, numbered `1` through `13`
#' graph <-
#'   create_graph() %>%
#'   add_node("a") %>%
#'   select_nodes() %>%
#'   add_n_nodes_ws(3, "from", "b") %>%
#'   clear_selection() %>%
#'   select_nodes("type == 'b'") %>%
#'   add_n_nodes_ws(3, "from", "c") %>%
#'   clear_selection()
#'
#' # Create a graph selection by selecting edges
#' # associated with nodes `1` and `2`
#' graph <-
#'   graph %>%
#'   select_edges_by_node_id(1:2)
#'
#' # Get the selection of edges
#' graph %>% get_selection()
#' #> "1 -> 2" "1 -> 3" "1 -> 4"
#'
#' # Perform another selection of nodes, with nodes
#' # `1`, `2`, and `4`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges_by_node_id(c(1, 2, 4))
#'
#' # Get the selection of edges
#' graph %>% get_selection
#' #> [1] "1 -> 2" "1 -> 3" "1 -> 4" "4 -> 5"
#' #> [5] "4 -> 6" "4 -> 7"
#'
#' # Get a fraction of the edges selected over all
#' # the edges in the graph
#' graph %>%
#' {
#'   l <- get_selection(.) %>%
#'     length(.)
#'   e <- edge_count(.)
#'   l/e
#' }
#' #> [1] 1
#' @export select_edges_by_node_id

select_edges_by_node_id <- function(graph,
                                    nodes,
                                    set_op = "union") {

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

  # Extract the edge data frame from the graph
  edge_df <- get_edge_df(graph)

  from <-
    edge_df[
      unique(c(which(edge_df$from %in% nodes),
               which(edge_df$to %in% nodes))),][,1]

  to <-
    edge_df[
      unique(c(which(edge_df$from %in% nodes),
               which(edge_df$to %in% nodes))),][,2]

  # Create selection of edges
  graph$selection$edges$from <- from
  graph$selection$edges$to <- to

  # Remove any selection of nodes
  graph$selection$nodes <- NULL

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_edges_by_node_id",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  return(graph)
}
