#' Add edges from an edge data frame to an existing graph object
#' @description With a graph object of class \code{dgr_graph} add edges from an
#' edge data frame to that graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_df an edge data frame that is created using \code{create_edges}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a graph with nodes and no edges
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                color = c("red", "green", "grey", "blue"),
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' graph <- create_graph(nodes_df = nodes)
#'
#' # Create an edge data frame
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to")
#'
#' # Add the edge data frame to the graph object to create a
#' # graph with both nodes and edges
#' graph <-
#'   add_edge_df(graph = graph, edge_df = edges)
#'
#' get_edges(graph, return_type = "vector")
#' #> [1] "a -> d" "b -> c" "c -> a"
#' }
#' @export add_edge_df

add_edge_df <- function(graph,
                        edge_df){

  if (is_graph_empty(graph) == TRUE){
    stop("Edges cannot be added to an empty graph.")
  }

  # Ensure that the nodes in the edge data frame specified are in
  # the graph object
  all_nodes_in_graph <-
    all(edge_df$from %in% get_nodes(graph)) &
    all(edge_df$to %in% get_nodes(graph))

  # If not all the nodes specified in the edge data frame are in the
  # graph, stop the function
  if (all_nodes_in_graph == FALSE){
    stop("Not all nodes specified in the edge data frame are in the graph.")
  }

  # If the 'edges_df' component of the graph is not null, combine the
  # incoming edge data frame with the existing edge definitions in the
  # graph object
  if (!is.null(graph$edges_df)){

    combined_edges <- combine_edges(graph$edges_df,
                                    edge_df)

    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = combined_edges,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }

  # If the 'edges_df' component of the graph is null, insert the
  # edge data frame into the graph object
  if (is.null(graph$edges_df)){

    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = edge_df,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }
}
