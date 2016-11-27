#' Select nodes in the graph based on their degree
#' values
#' @description Using a graph object of class
#' \code{dgr_graph}, create a selection of nodes
#' that have certain degree values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param expressions one or more expressions for
#' filtering of nodes by degree values. Use a
#' combination of a degree type (\code{deg} for
#' total degree, \code{indeg} for indegree, and
#' \code{outdeg} for outdegree) with a comparison
#' operator and values for comparison (e.g., use
#' \code{"deg >= 2"} to select nodes with a degree
#' greater than or equal to 2).
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph with a high amount
#' # of connectedness
#' graph <-
#'   create_random_graph(
#'     n = 35, m = 125,
#'     set_seed = 23)
#'
#' # Report which nodes have a total degree (indegree
#' # + outdegree) of exactly 9
#' graph %>%
#'   select_nodes_by_degree("deg == 9") %>%
#'   get_selection()
#' #> [1]  2  9 10 14 17 19 31 33
#'
#' # Report which nodes have a total degree greater
#' # than or equal to 9
#' graph %>%
#'   select_nodes_by_degree("deg >= 9") %>%
#'   get_selection()
#' #> [1]  2  6  9 10 14 17 19 22 25 29 31 33
#'
#' # Combine two calls of `select_nodes_by_degree()`
#' # to get those nodes with total degree less than
#' # 3 and total degree greater than 10 (by default,
#' # those `select...()` functions `union` the sets
#' # of nodes selected)
#' graph %>%
#'   select_nodes_by_degree("deg < 3") %>%
#'   select_nodes_by_degree("deg > 10") %>%
#'   get_selection()
#' #> [1]  6 16 22
#'
#' # Combine two calls of `select_nodes_by_degree()`
#' # to get those nodes with total degree greater than
#' # or equal to 3 and less than or equal to 10 (the
#' # key here is to `intersect` the sets of nodes
#' # selected in the second call)
#' graph %>%
#'   select_nodes_by_degree("deg >= 3") %>%
#'   select_nodes_by_degree("deg <= 10", "intersect") %>%
#'   get_selection()
#' #>  [1]  1  2  3  4  5  7  8  9 10 11 12 13 14
#' #> [14] 15 17 18 19 20 21 23 24 25 26 27 28 29
#' #> [28] 30 31 32 33 34 35
#'
#' # Select all nodes with an indegree greater than 5,
#' # then, apply a node attribute to those selected nodes
#' # (coloring the selected nodes red)
#' graph_2 <-
#'   graph %>%
#'   select_nodes_by_degree("indeg > 5") %>%
#'   set_node_attrs_ws("color", "red")
#'
#' # Get the selection of nodes
#' graph_2 %>% get_selection()
#' #> [1] 14 22 23 25 27 29 31 33 34
#' @importFrom dplyr select filter_
#' @export select_nodes_by_degree

select_nodes_by_degree <- function(graph,
                                   expressions,
                                   set_op = "union") {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no selections can be made.")
  }

  # Create bindings for specific variables
  id <- deg <- indeg <- outdeg <- NULL

  # Get a data frame with node ID and degree types
  node_degree <-
    node_info(graph) %>%
    dplyr::select(id, deg, indeg, outdeg)

  for (i in 1:length(expressions)) {
    node_degree <-
      node_degree %>%
      dplyr::filter_(expressions[i])
  }

  # Get the node ID values from the filtered table
  nodes_selected <- node_degree$id

  # If no node ID values in `nodes_selected` return
  # the graph without a changed node selection
  if (length(nodes_selected) == 0) {
    return(graph)
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  # Incorporate selected nodes into graph's
  # selection section
  if (set_op == "union") {
    nodes_combined <-
      union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect") {
    nodes_combined <-
      intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference") {
    nodes_combined <-
      setdiff(nodes_prev_selection, nodes_selected)
  }

  # Add the node ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <-
    replace_graph_node_selection(
      graph = graph,
      replacement = nodes_combined)

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "select_nodes_by_degree",
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
