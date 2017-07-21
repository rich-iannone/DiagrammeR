#' Add nodes from a node data frame to an existing
#' graph object
#' @description With a graph object of class
#' \code{dgr_graph} add nodes from a node data frame to
#' that graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_df a node data frame that is created
#' using \code{create_node_df}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     color = c("red", "green", "grey", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Add the node data frame to the graph object to
#' # create a graph with nodes
#' graph <-
#'   add_node_df(graph, node_df = ndf)
#'
#' get_node_df(graph)
#' #>   id  type label color value
#' #> 1  1 basic  <NA>   red   3.5
#' #> 2  2 basic  <NA> green   2.6
#' #> 3  3 basic  <NA>  grey   9.4
#' #> 4  4 basic  <NA>  blue   2.7
#'
#' # Create another node data frame
#' ndf_2 <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     color = c("white", "brown", "aqua", "pink"),
#'     value = c(1.6, 6.4, 0.8, 4.2))
#'
#' # Add the second node data frame to the graph object
#' # to add more nodes with attributes to the graph
#' graph <-
#'   add_node_df(graph, node_df = ndf_2)
#'
#' # View the graph's internal node data frame using
#' # the `get_node_df()` function
#' get_node_df(graph)
#' #>   id  type label color value
#' #> 1  1 basic  <NA>   red   3.5
#' #> 2  2 basic  <NA> green   2.6
#' #> 3  3 basic  <NA>  grey   9.4
#' #> 4  4 basic  <NA>  blue   2.7
#' #> 5  5 basic  <NA> white   1.6
#' #> 6  6 basic  <NA> brown   6.4
#' #> 7  7 basic  <NA>  aqua   0.8
#' #> 8  8 basic  <NA>  pink   4.2
#' @importFrom dplyr bind_rows
#' @export add_node_df

add_node_df <- function(graph,
                        node_df) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Combine the incoming node data frame with the
  # existing node definitions in the graph object
  node_df[, 1] <-
    as.integer(nodes_created + seq(1:nrow(node_df)))

  node_df[, 2] <- as.character(node_df[, 2])
  node_df[, 3] <- as.character(node_df[, 3])

  graph$nodes_df <-
    dplyr::bind_rows(
      graph$nodes_df, node_df)

  # Update the `last_node` counter
  graph$last_node <-
    nodes_created + nrow(node_df)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_node_df",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Perform graph actions, if any are available
  if (nrow(graph$graph_actions) > 0) {
    graph <-
      graph %>%
      trigger_graph_actions()
  }

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
