#' Select nodes based on a walk distance from a specified node
#' @description Select those nodes in the neighborhood of nodes connected
#' a specified distance from an initial node.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param node the node from which the traversal will originate.
#' @param distance the maximum number of steps from the \code{node}
#' for inclusion in the subgraph.
#' @param set_op the set operation to perform upon consecutive selections
#' of graph nodes. This can either be as a \code{union} (the default), as an
#' \code{intersection}, or, as a \code{difference} on the previous selection,
#' if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a graph, then, create a subgraph of that larger graph
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                type = "letter",
#'                shape = sample(c("circle", "rectangle"),
#'                               length(LETTERS),
#'                               replace = TRUE),
#'                fillcolor = sample(c("aqua", "red",
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
#' # Create a selection of nodes centered around node "U" and
#' # including those nodes a depth of 2 edges away
#' graph <-
#'   select_nodes_in_neighborhood(graph = graph,
#'                                node = "U",
#'                                distance = 2)
#' }
#' @export select_nodes_in_neighborhood

select_nodes_in_neighborhood <- function(graph,
                                         node,
                                         distance,
                                         set_op = "union"){

  # Create and empty list object
  nodes <- list()

  # Find nodes belonging to the neighborhood
  for (i in 1:distance){

    if (i == 1){

      nodes[[i]] <- vector(mode = "character")

      nodes[[i]] <-
        c(as.character(get_edges(graph,
                                 return_type = "df")[
                                   which(get_edges(graph,
                                                   return_type = "df")[,1] ==
                                           node),2]),
          as.character(get_edges(graph,
                                 return_type = "df")[
                                   which(get_edges(graph,
                                                   return_type = "df")[,2] ==
                                           node),1]))
    }

    if (i > 1){

      for (j in 1:length(nodes[[i-1]])){

        if (j == 1) nodes[[i]] <- vector(mode = "character")

        nodes[[i]] <-
          c(nodes[[i]],
            as.character(get_edges(graph,
                                   return_type = "df")[
                                     which(get_edges(graph,
                                                     return_type = "df")[,1] ==
                                             nodes[[i-1]][j]),2]),
            as.character(get_edges(graph,
                                   return_type = "df")[
                                     which(get_edges(graph,
                                                     return_type = "df")[,2] ==
                                             nodes[[i-1]][j]),1]))
      }
    }
  }

  # From list of nodes, obtain vector of unique nodes as neighbors
  nodes_selected <- unique(unlist(nodes))

  # Obtain vector of node IDs selection of nodes already present
  if (!is.null(graph$selection)){
    if (!is.null(graph$selection$nodes)){
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected nodes into graph's selection section
  if (set_op == "union"){
    nodes_combined <- union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect"){
    nodes_combined <- intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference"){
    nodes_combined <- setdiff(nodes_prev_selection, nodes_selected)
  }

  graph$selection$nodes <- nodes_combined

  return(graph)
}
