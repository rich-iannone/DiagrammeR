#' Set edge attributes
#' @description From a graph object of class \code{dgr_graph} or an edge
#' data frame, set edge attribute properties for one or more edges
#' @param x either a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}, or an edge data frame.
#' @param from an optional vector of node IDs from which the edge is outgoing
#' for filtering list of nodes with outgoing edges in the graph.
#' @param to an optional vector of node IDs from which the edge is incoming
#' for filtering list of nodes with incoming edges in the graph.
#' @param edge_attr the name of the attribute to set.
#' @param values the values to be set for the chosen attribute for the
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
#'                 edge_attr = "color", values = "green")
#'
#' # Set attribute 'color = "green"' for edges "a" -> "d"
#' # and "c" -> "a" using the edge data frame
#' edges <-
#'   set_edge_attr(x = edges,
#'                 from = c("a", "c"),
#'                 to = c("d", "a"),
#'                 edge_attr = "color", values = "green")
#'
#' # Set attribute 'color = "blue"' for all edges in graph
#' graph <-
#'   set_edge_attr(x = graph,
#'                 edge_attr = "color", values = "blue")
#'
#' # Set attribute 'color = "pink"' for all edges in graph
#' # outbound from "a"
#' graph <-
#'   set_edge_attr(x = graph,
#'                 from = "a",
#'                 edge_attr = "color", values = "pink")
#'
#' # Set attribute 'color = "black"' for all edges in graph
#' # inbound to "a"
#' graph <-
#'   set_edge_attr(x = graph,
#'                 to = "a",
#'                 edge_attr = "color", values = "black")
#' }
#' @export set_edge_attr

set_edge_attr <- function(x,
                          from = NULL,
                          to = NULL,
                          edge_attr,
                          values){

  if (edge_attr == "from" | edge_attr == "to"){
    stop("You cannot alter values associated with node IDs.")
  }

  if (!is.null(from) & !is.null(to)){
    if (length(from) != length(to)){
      stop("The number of nodes 'from' and 'to' must be the same.")
    }
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

  if (length(values) != 1 & length(values) != nrow(edges_df)){
    stop("The length of values provided must either be 1 or that of the number of rows in the edf.")
  }

  if (length(values) == 1){

    if (edge_attr %in% colnames(edges_df)){

      if (is.null(from) & !is.null(to)){

        edges_df[which(edges_df$to %in% to),
                 which(colnames(edges_df) %in% edge_attr)] <- values

      } else if (!is.null(from) & is.null(to)){

        edges_df[which(edges_df$from %in% from),
                 which(colnames(edges_df) %in% edge_attr)] <- values

      } else if (is.null(from) & is.null(to)){

        edges_df[, which(colnames(edges_df) %in% edge_attr)] <- values

      } else {

        edges_df[which((edges_df$from %in% from) &
                         (edges_df$to %in% to)),
                 which(colnames(edges_df) %in% edge_attr)] <- values
      }
    }

    if (!(edge_attr %in% colnames(edges_df))){

      edges_df <- cbind(edges_df, rep("", nrow(edges_df)))

      edges_df[, ncol(edges_df)] <- as.character(edges_df[,ncol(edges_df)])

      colnames(edges_df)[ncol(edges_df)] <- edge_attr

      if (is.null(from) & !is.null(to)){

        edges_df[which(edges_df$to %in% to),
                 ncol(edges_df)] <- values

      } else if (!is.null(from) & is.null(to)){

        edges_df[which(edges_df$from %in% from),
                 ncol(edges_df)] <- values

      } else if (is.null(from) & is.null(to)){

        edges_df[, ncol(edges_df)] <- values

      } else {

        edges_df[which((edges_df$from %in% from) &
                         (edges_df$to %in% to)),
                 ncol(edges_df)] <- values
      }
    }
  }

  if (length(values) == nrow(edges_df)){

    if (edge_attr %in% colnames(edges_df)){

      edges_df[, which(colnames(edges_df) %in% edge_attr)] <- values
    }

    if (!(edge_attr %in% colnames(edges_df))){

      edges_df <- cbind(edges_df, rep("", nrow(edges_df)))

      edges_df[, ncol(edges_df)] <- as.character(edges_df[,ncol(edges_df)])

      colnames(edges_df)[ncol(edges_df)] <- edge_attr

      edges_df[, ncol(edges_df)] <- values
    }
  }

  if (object_type == "dgr_graph"){

    # Create new graph object
    dgr_graph <-
      create_graph(nodes_df = x$nodes_df,
                   edges_df = edges_df,
                   graph_attrs = x$graph_attrs,
                   node_attrs = x$node_attrs,
                   edge_attrs = x$edge_attrs,
                   directed = ifelse(is_graph_directed(x),
                                     TRUE, FALSE),
                   graph_name = x$graph_name,
                   graph_time = x$graph_time,
                   graph_tz = x$graph_tz)

    return(dgr_graph)
  }

  if (object_type == "edge_df"){
    return(edges_df)
  }
}
