#' Add nodes from a node data frame to an existing
#' graph object
#' @description With a graph object of class
#' \code{dgr_graph} add nodes from a node data frame to
#' that graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_df a node data frame that is created
#' using \code{create_nodes}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Create a node data frame
#' nodes <-
#'   create_nodes(
#'     nodes = 1:4,
#'     type = "basic",
#'     color = c("red", "green", "grey", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Add the node data frame to the graph object to
#' # create a graph with nodes
#' graph <- add_node_df(graph, nodes)
#'
#' get_node_df(graph)
#' #>   nodes  type label color value
#' #> 1     1 basic     a   red   3.5
#' #> 2     2 basic     b green   2.6
#' #> 3     3 basic     c  grey   9.4
#' #> 4     4 basic     d  blue   2.7
#'
#' # Create another node data frame
#' nodes_2 <-
#'   create_nodes(
#'     nodes = 5:8,
#'     type = "basic",
#'     color = c("white", "brown", "aqua", "pink"),
#'     value = c(1.6, 6.4, 0.8, 4.2))
#'
#' # Add the second node data frame to the graph object
#' # to add more nodes with attributes to the graph
#' graph <- add_node_df(graph, nodes_2)
#'
#' get_node_df(graph)
#' #>   nodes  type label color value
#' #> 1     1 basic     a   red   3.5
#' #> 2     2 basic     b green   2.6
#' #> 3     3 basic     c  grey   9.4
#' #> 4     4 basic     d  blue   2.7
#' #> 5     5 basic     e white   1.6
#' #> 6     6 basic     f brown   6.4
#' #> 7     7 basic     g  aqua   0.8
#' #> 8     8 basic     h  pink   4.2
#' @export add_node_df

add_node_df <- function(graph,
                        node_df) {

  # Ensure that the nodes in the node data frame
  # specified are not in the graph object
  all_nodes_not_in_graph <-
    all(!(node_df$nodes %in% get_node_ids(graph)))

  # If not all the nodes specified in the node data
  # frame are in the graph, stop the function
  if (all_nodes_not_in_graph == FALSE) {
    stop("One or more of the nodes specified are already in the graph.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # If the `nodes_df` component of the graph is not
  # NULL, combine the incoming node data frame with the
  # existing node definitions in the graph object
  if (!is.null(graph$nodes_df)) {

    combined_nodes <-
      combine_nodes(graph$nodes_df,
                    node_df)

    dgr_graph <-
      create_graph(
        nodes_df = combined_nodes,
        edges_df = graph$edges_df,
        graph_attrs = graph$graph_attrs,
        node_attrs = graph$node_attrs,
        edge_attrs = graph$edge_attrs,
        directed = ifelse(is_graph_directed(graph),
                          TRUE, FALSE),
        graph_name = graph$graph_name,
        graph_time = graph$graph_time,
        graph_tz = graph$graph_tz)

    # Update the `last_node` counter
    graph$last_node <-
      nodes_created + nrow(node_df)

    return(dgr_graph)
  }

  # If the `nodes_df` component of the graph is NULL,
  # insert the node data frame into the graph object
  if (is.null(graph$nodes_df)) {

    dgr_graph <-
      create_graph(
        nodes_df = node_df,
        edges_df = graph$edges_df,
        graph_attrs = graph$graph_attrs,
        node_attrs = graph$node_attrs,
        edge_attrs = graph$edge_attrs,
        directed = ifelse(is_graph_directed(graph),
                          TRUE, FALSE),
        graph_name = graph$graph_name,
        graph_time = graph$graph_time,
        graph_tz = graph$graph_tz)

    # Update the `last_node` counter
    graph$last_node <-
      nodes_created + nrow(node_df)

    return(dgr_graph)
  }
}
