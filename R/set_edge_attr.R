#' Set edge attributes
#' @description From a graph object of class \code{dgr_graph} or an edge
#' data frame, set edge attribute properties for one or more edges
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}, or an edge data frame.
#' @param from a vector of node IDs from which the edge is outgoing, or,
#' the string "*" to select all outgoing nodes in the graph or node data
#' frame.
#' @param to a vector of node IDs to which the edge is incoming, or,
#' the string "*" to select all incoming nodes in the graph or node data
#' frame.
#' @param attr the name of the attribute to set.
#' @param value the value to be set for the chosen attribute for the
#' chosen edges.
#' @return either a graph object of class \code{dgr_graph} or an edge
#' data frame, depending on what type of object was supplied to \code{x}.
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
#'                rel = "leading_to")
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Set attribute 'color = "green"' for edges "a" -> "d"
#' # and "c" -> "a" using the graph object
#' graph <-
#'   set_edge_attr(x = graph,
#'                 from = c("a", "c"),
#'                 to = c("d", "a"),
#'                 attr = "color", value = "green")
#'
#' # Set attribute 'color = "green"' for edges "a" -> "d"
#' # and "c" -> "a" using the node data frame
#' edges <-
#'   set_edge_attr(x = edges,
#'                 from = c("a", "c"),
#'                 to = c("d", "a"),
#'                 attr = "color", value = "green")
#' }
#' @export set_edge_attr

set_edge_attr <- function(x,
                          from,
                          to,
                          attr,
                          value){

  if (attr == "from" | attr == "to"){
    stop("You cannot alter values associated with node IDs.")
  }

  if (length(value) > 1){
    stop("Only one value should be provided.")
  }

  if (length(from) != length(to)){
    stop("The number of nodes 'from' and 'to' must be the same.")
  }

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

  if (attr %in% colnames(edges_df)){

    edges_df[which((edges_df$from %in% from) &
                     (edges_df$to %in% to)),
             which(colnames(edges_df) %in% attr)] <- value
  }


  if (!(attr %in% colnames(edges_df))){

    edges_df <- cbind(edges, rep("", nrow(edges_df)))

    edges_df[,ncol(edges_df)] <- as.character(edges_df[,ncol(edges_df)])

    colnames(edges_df)[ncol(edges_df)] <- attr

    edges_df[which((edges_df$from %in% from) &
                     (edges_df$to %in% to)),ncol(edges_df)] <- value
  }

  if (object_type == "dgr_graph"){

    # Create new graph object
    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = edges_df,
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

  if (object_type == "edge_df"){
    return(edges_df)
  }
}
