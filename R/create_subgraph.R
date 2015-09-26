#' Create a subgraph based on a walk distance from a specified node
#' @description Create a subgraph for a neighborhood of nodes connected a
#' specified distance from the selected node.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param starting_node the node from which the subgraph will originate.
#' @param distance the maximum number of steps from the \code{starting_node}
#' for inclusion in the subgraph.
#' @examples
#' \dontrun{
#' # Create a graph, then, create a subgraph of that larger graph
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                type = "letter",
#'                shape = sample(c("circle", "rectangle"),
#'                               length(LETTERS),
#'                               replace = TRUE),
#'                fillcolor = sample(c("aqua", "gray80",
#'                                     "pink", "lightgreen",
#'                                     "azure", "yellow"),
#'                                   length(LETTERS),
#'                                   replace = TRUE))
#'
#' edges <-
#'   create_edges(from = sample(LETTERS, replace = TRUE),
#'                to = sample(LETTERS, replace = TRUE),
#'                rel = "letter_to_letter")
#'
#' graph <- create_graph(nodes_df = nodes,
#'                       edges_df = edges,
#'                       graph_attrs = "layout = neato",
#'                       node_attrs = c("fontname = Helvetica",
#'                                      "style = filled"),
#'                       edge_attrs = c("color = gray20",
#'                                      "arrowsize = 0.5"))
#'
#' # Create a subgraph centered around node "U" and include
#' # those nodes up to (and including) 2 connections away
#' subgraph <- create_subgraph(graph = graph,
#'                             starting_node = "U",
#'                             distance = 2)
#'
#' # Render the graph using Graphviz
#' render_graph(subgraph)
#'
#' # Render the graph using VivaGraph
#' render_graph(subgraph, output = "vivagraph")
#' }
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

  # Get a node data frame that doesn't contain the excluded nodes
  nodes_df <-
    graph$nodes_df[which(!(graph$nodes_df$nodes %in% excluded_nodes)),]


  # Get an edge data frame that doesn't contain the excluded nodes
  edges_df <-
    graph$edges_df[which(!(graph$edges_df$from %in% excluded_nodes) &
                           !(graph$edges_df$to %in% excluded_nodes)),]

  # Create a subgraph based on the neighborhood
  subgraph <-
    create_graph(nodes_df = nodes_df,
                 edges_df = edges_df,
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
