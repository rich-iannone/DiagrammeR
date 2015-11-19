#' Get edge attributes
#' @description From a graph object of class \code{dgr_graph}, get edge
#' attribute properties for one or more edges
#' @param graph a graph object of class \code{dgr_graph}.
#' @param edge_attr the edge attribute from which to obtain values.
#' @param mode a option to recast the returned vector of edge attribute
#' value as \code{numeric} or \code{character}.
#' @param from an optional vector of node IDs from which the edge is
#' outgoing for filtering the list of edges present in the graph.
#' @param to an optional vector of node IDs to which the edge is
#' incoming for filtering the list of edges present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to",
#'                color = c("pink", "blue", "red"))
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Get edge attributes for all edges in the graph
#' get_edge_attr(x = graph)
#'
#' # Get edge attributes for edges "a" -> "d" and "c" -> "a" in
#' # the graph
#' get_edge_attr(x = graph,
#'               from = c("a", "c"), to = c("d", "a"))
#'
#' # Get edge attributes for all edges in graph outbound
#' # from "a"
#' get_edge_attr(x = graph,
#'               from = "a")
#'
#' # Get edge attributes for all edges in graph inbound
#' # to "a"
#' get_edge_attr(x = graph,
#'               to = "a")
#' }
#' @export get_edge_attr

get_edge_attr <- function(graph,
                          edge_attr,
                          mode = NULL,
                          from = NULL,
                          to = NULL){

  if (class(x) == "dgr_graph"){

    object_type <- "dgr_graph"

    edges_df <- x$edges_df
  }

  if (class(x) == "data.frame"){

    if (all(c("from", "to") %in% colnames(x))){

      object_type <- "edge_df"
      edges_df <- x
    }
  }

  if (is.null(from) & !is.null(to)){

    edges_df <-
      edges_df[which(edges_df$to %in% to),]

  } else if (!is.null(from) & is.null(to)){

    edges_df <-
      edges_df[which(edges_df$from %in% from),]

  } else if (is.null(from) & is.null(to)){

    edges_df <- edges_df

  } else {

    edges_df <-
      edges_df[which((edges_df$from %in% from) &
                       (edges_df$to %in% to)),]
  }

  if (is.null(edge_attr)){
    return(edges_df)
  }

  if (!is.null(edge_attr)){

    if (any(edge_attr %in% colnames(edges_df)[-c(1:2)])){

      edges_attr_vector <-
        edges_df[,which(colnames(edges_df) %in% edge_attr)]

      if (!is.null(mode)){
        if (mode == "numeric"){
          edges_attr_vector <- as.numeric(edges_attr_vector)

          edges_attr_vector <-
            edges_attr_vector[which(!is.na(edges_attr_vector))]
        }

        if (mode == "character"){
          edges_attr_vector <- as.character(edges_attr_vector)
        }
      }
    }

    graph$deposit <- edges_attr_vector

    return(graph)
  }
}
