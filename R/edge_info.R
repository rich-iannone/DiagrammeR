#' Get detailed information on edges
#' @description Obtain a data frame with detailed information on edges and
#' their interrelationships within a graph.
#' @param graph a graph object of class \code{dgr_graph}.
#' @return a data frame containing information specific to each edge within
#' the graph.
#' @examples
#' \dontrun{
#' # Create a simple graph and get edge information from it
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
#' edge_info(graph)
#' #>    from   to              rel label
#' #> 1     A    Z letter_to_letter  edge
#' #> 2     H    U letter_to_letter  edge
#' #> 3     W    O letter_to_letter  edge
#' #> 4     U    K letter_to_letter  edge
#' #> 5     I    V letter_to_letter  edge
#' #>..   ...  ...              ...   ...
#' }
#' @export edge_info

edge_info <- function(graph){

  #   if ("edge_from" %in% colnames(graph$edges_df)){
  #     edge_from <- graph$edges_df$edge_from
  #   }

  if ("from" %in% colnames(graph$edges_df)){
    edge_from <- graph$edges_df$from
  }

  #   if ("edge_to" %in% colnames(graph$edges_df)){
  #     edge_to <- graph$edges_df$edge_to
  #   }

  if ("to" %in% colnames(graph$edges_df)){
    edge_to <- graph$edges_df$to
  }

  if ("label" %in% colnames(graph$edges_df)){
    label <- graph$edges_df$label
  }

  if ("rel" %in% colnames(graph$edges_df)){
    rel <- graph$edges_df$rel
  }

  # For graphs with no edges, create an 'edge_properties' data frame
  # that doesn't need to consider any edge information
  if (is.null(graph$edges_df)){

    edge_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 4))
    colnames(edge_properties) <- c("from", "to", "rel", "label")

    return(edge_properties)
  }

  # For graphs with no edges, create an 'edge_properties' data frame
  if (!is.null(graph$edges_df)){

    # Create data frame of edge properties
    for (i in 1:length(edge_from)){

      if (i == 1){
        edge_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 4))
        colnames(edge_properties) <- c("from", "to", "rel", "label")
      }

      # Collect information into the 'edge_properties' data frame
      edge_properties[i, 1] <- edge_from[i]
      edge_properties[i, 2] <- edge_to[i]
      edge_properties[i, 3] <-
        ifelse(exists("rel"),
               rel[which((edge_from %in% edge_from[i]) &
                           (edge_to %in% edge_to[i]))],
               rep(NA, length(edge_from)))
      edge_properties[i, 4] <-
        ifelse(exists("label"),
               label[which((edge_from %in% edge_from[i]) &
                             (edge_to %in% edge_to[i]))],
               rep(NA, length(edge_from)))
    }

    return(edge_properties)
  }
}
