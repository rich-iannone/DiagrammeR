#' Rename a node attribute
#' @description Within a graph's internal node data
#' frame (ndf), rename an existing node attribute.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr_from the name of the node attribute
#' that will be renamed.
#' @param node_attr_to the new name of the node
#' attribute column identified in \code{node_attr_from}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 5, m = 10,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = "shape",
#'     values = "circle")
#'
#' # Get the graph's internal ndf to show which
#' # node attributes are available
#' get_node_df(graph)
#' #>   id type label value  shape
#' #> 1  1 <NA>     1   6.0 circle
#' #> 2  2 <NA>     2   2.5 circle
#' #> 3  3 <NA>     3   3.5 circle
#' #> 4  4 <NA>     4   7.5 circle
#' #> 5  5 <NA>     5   8.5 circle
#'
#' # Rename the `value` node attribute as `weight`
#' graph <-
#'   graph %>%
#'   rename_node_attrs(
#'     node_attr_from = "value",
#'     node_attr_to = "weight")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute had been renamed
#' get_node_df(graph)
#' #>   id type label weight  shape
#' #> 1  1 <NA>     1    6.0 circle
#' #> 2  2 <NA>     2    2.5 circle
#' #> 3  3 <NA>     3    3.5 circle
#' #> 4  4 <NA>     4    7.5 circle
#' #> 5  5 <NA>     5    8.5 circle
#' @export rename_node_attrs

rename_node_attrs <- function(graph,
                              node_attr_from,
                              node_attr_to) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no node attributes can be renamed.")
  }

  # Stop function if `node_attr_from` and
  # `node_attr_to` are identical
  if (node_attr_from == node_attr_to) {
    stop("You cannot rename using the same name.")
  }

  # Stop function if `node_attr_to` is `id` or any
  # other column name in the graph's node data frame
  if (any(unique(c("id",
                   colnames(get_node_df(graph)))) %in%
          node_attr_to)) {
    stop("You cannot use that name for `node_attr_to`.")
  }

  # Stop function if `node_attr_from` is `id`, `label`
  # or `type`
  if (any(c("id", "label", "type") %in%
          node_attr_from)) {
    stop("You cannot use that name for `node_attr_from`.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's columns
  if (!any(column_names_graph %in% node_attr_from)) {
    stop("The node attribute to rename is not in the ndf.")
  }

  # Set the column name for the renamed attr
  colnames(nodes)[
    which(colnames(nodes) %in%
            node_attr_from)] <- node_attr_to

  # Modify the graph's ndf
  graph$nodes_df <- nodes

  # Update the `last_node` counter
  graph$last_node <- nodes_created

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "rename_node_attrs",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
