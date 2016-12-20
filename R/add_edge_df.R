#' Add edges from an edge data frame to an existing
#' graph object
#' @description With a graph object of class
#' \code{dgr_graph} add edges from an edge data frame
#' to that graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_df an edge data frame that is created
#' using \code{create_edge_df}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "letter",
#'     color = c("red", "green", "grey", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create a graph with nodes and no edges
#' graph <- create_graph(nodes_df = ndf)
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Add the edge data frame to the graph object to
#' # create a graph with both nodes and edges
#' graph <-
#'   graph %>%
#'   add_edge_df(edf)
#'
#' # Get the graph's edges to verify that the edge
#' # data frame had been added
#' get_edges(graph, return_type = "vector")
#' #> [1] "1->4" "2->3" "3->1"
#' @export add_edge_df

add_edge_df <- function(graph,
                        edge_df) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, edges cannot be added.")
  }

  # Get the number of edges ever created for
  # this graph
  edges_created <- graph$last_edge

  # If not all the nodes specified in the edge data
  # frame are in the graph, stop the function
  # if (all(unique(c(edge_df$from, edge_df$to)) %in%
  #         get_node_ids(graph)) == FALSE) {
  #   stop("Not all nodes specified in the edge data frame are in the graph.")
  # }

  # Combine the incoming edge data frame
  # with those in the graph
  combined_edges <-
    combine_edfs(
      graph$edges_df,
      edge_df)

  # Replace the graph's internal edge
  # data frame with the `combined_edges`
  # edge data frame
  graph$edges_df <- combined_edges

  # Update the `last_edge` counter
  graph$last_edge <- edges_created + nrow(combined_edges)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_edge_df",
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
