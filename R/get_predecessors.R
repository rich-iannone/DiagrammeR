#' Get node IDs for predecessor nodes to the specified node
#' @description Provides a vector of node IDs for all nodes that have a
#' connection to the given node.
#' @param graph a graph object of class \code{dgr_graph}.
#' @param node a node ID for the selected node.
#' @return a vector of node ID values.
#' @examples
#' \dontrun{
#' # Before getting node ID values for predecessors of
#' # a specified node, create a simple graph
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                label = TRUE,
#'                type = c(rep("a_to_g", 7),
#'                         rep("h_to_p", 9),
#'                         rep("q_to_x", 8),
#'                         rep("y_and_z",2)))
#'
#' edges <-
#'   create_edges(from = sample(LETTERS, replace = TRUE),
#'                to = sample(LETTERS, replace = TRUE),
#'                label = "edge",
#'                rel = "letter_to_letter")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges,
#'                graph_attrs = "layout = neato",
#'                node_attrs = c("fontname = Helvetica",
#'                               "shape = circle"))
#'
#' # Get predecessors for node "Z" in the graph
#' get_predecessors(graph, node = "Z")
#' #> [1] "A" "R" "R"
#'
#' # If there are no predecessors, NA is returned
#' get_predecessors(graph, node = "A")
#' #> [1] NA
#' }
#' @export get_predecessors

get_predecessors <- function(graph,
                             node){

  # Determine whether graph has nodes
  graph_is_not_empty <- !is_graph_empty(graph)

  # Determine whether the node is in the graph
  node_is_in_graph <- node_present(graph, node)

  # Obtain the node's predecessors
  if (graph_is_not_empty & node_is_in_graph & nrow(edge_info(graph)) > 0){

    if (length(graph$edges_df[graph$edges_df$to == node,]$from) == 0){

      predecessors <- NA

    } else {

      predecessors <- graph$edges_df[graph$edges_df$to == node,]$from

    }

    return(predecessors)
  }
}
