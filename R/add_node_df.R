#' Add nodes from a node data frame to an existing graph object
#'
#' With a graph object of class \code{dgr_graph} add nodes from a node data
#'   frame to that graph.
#' @inheritParams render_graph
#' @param node_df a node data frame that is created using
#' \code{\link{create_node_df}()}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(n = 2)
#'
#' # Add the node data frame to
#' # the graph object to create
#' # a graph with nodes
#' graph <-
#'   graph %>%
#'   add_node_df(
#'     node_df = ndf)
#'
#' # Inspect the graph's ndf
#' graph %>%
#'   get_node_df()
#'
#' # Create another ndf
#' ndf_2 <-
#'   create_node_df(n = 3)
#'
#' # Add the second node data
#' # frame to the graph object
#' # to add more nodes with
#' # attributes to the graph
#' graph <-
#'   graph %>%
#'   add_node_df(
#'     node_df = ndf_2)
#'
#' # View the graph's internal
#' # node data frame using the
#' # `get_node_df()` function
#' graph %>%
#'   get_node_df()
#' @importFrom dplyr bind_rows
#' @export
add_node_df <- function(graph,
                        node_df) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

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

  # Get the updated number of nodes in the graph
  nodes_graph_2 <- graph %>% count_nodes()

  # Get the number of nodes added to
  # the graph
  nodes_added <- nodes_graph_2 - nodes_graph_1

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nodes_added)

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
