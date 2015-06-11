#' Create a subgraph based on a walk distance from a specified node
#' @description Create a subgraph for a neighborhood of nodes connected a specified distance from the selected node.
#' @param graph a graph object of class \code{dgr_graph} that is created using \code{create_graph}.
#' @param starting_node the node from which the subgraph will originate.
#' @param distance the maximum number of steps from the \code{starting_node} for inclusion in the subgraph.
#' @export create_subgraph

create_subgraph <- function(graph,
                            starting_node,
                            distance){

  # Create and empty list object
  nodes <- list()

  # Find nodes belonging to the neighborhood
  for (i in 1:distance){

    if (i == 1){

      nodes[[i]] <- vector(mode = "character")

      nodes[[i]] <-
        c(as.character(get_edges(graph = graph,
                                 return_type = "df")[
                                   which(get_edges(graph = graph,
                                                   return_type = "df")[,1] ==
                                           starting_node),2]),
          as.character(get_edges(graph = graph,
                                 return_type = "df")[
                                   which(get_edges(graph = graph,
                                                   return_type = "df")[,2] ==
                                           starting_node),1]))
    }

    if (i > 1){

      for (j in 1:length(nodes[[i-1]])){

        if (j == 1) nodes[[i]] <- vector(mode = "character")

        nodes[[i]] <-
          c(nodes[[i]],
            as.character(get_edges(graph = graph,
                                   return_type = "df")[
                                     which(get_edges(graph = graph,
                                                     return_type = "df")[,1] ==
                                             nodes[[i-1]][j]),2]),
            as.character(get_edges(graph = graph,
                                   return_type = "df")[
                                     which(get_edges(graph = graph,
                                                     return_type = "df")[,2] ==
                                             nodes[[i-1]][j]),1]))
      }
    }
  }

  # From list of nodes, obtain vector of unique nodes as neighbors
  nodes_in_neighbourhood <- unique(unlist(nodes))

  # Determine which nodes are excluded from this neighborhood
  excluded_nodes <-
    get_nodes(graph = graph)[which(!(get_nodes(graph = graph) %in%
                                       nodes_in_neighbourhood))]

  # Create a subgraph based on the neighborhood
  subgraph <-
    create_graph(nodes_df = subset(graph$nodes_df,
                                   !(nodes %in% excluded_nodes)),
                 edges_df = subset(graph$edges_df,
                                   !(from %in% excluded_nodes) &
                                     !(to %in% excluded_nodes)),
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  # Return the subgraph
  return(subgraph)
}
