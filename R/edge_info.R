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
#' #>    from   to              rel
#' #> 1     A    Z letter_to_letter
#' #> 2     H    U letter_to_letter
#' #> 3     W    O letter_to_letter
#' #> 4     U    K letter_to_letter
#' #> 5     I    V letter_to_letter
#' #>..   ...  ...              ...
#' }
#' @export edge_info

edge_info <- function(graph){

  if ("from" %in% colnames(graph$edges_df)){
    edge_from <- graph$edges_df$from
  }

  if ("to" %in% colnames(graph$edges_df)){
    edge_to <- graph$edges_df$to
  }

  if ("rel" %in% colnames(graph$edges_df)){
    relationship <- graph$edges_df$rel
  }

  # For graphs with no edges, create an 'edge_properties' data frame
  # that doesn't need to consider any edge information
  if (is.null(graph$edges_df)){

    edge_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 3))
    colnames(edge_properties) <- c("from", "to", "rel")

    return(edge_properties)
  }

  # For graphs with no edges, create an 'edge_properties' data frame
  if (!is.null(graph$edges_df)){

    # Create data frame of edge properties
    for (i in 1:length(edge_from)){

      if (i == 1){
        edge_properties <- as.data.frame(mat.or.vec(nr = 0, nc = 3))
        colnames(edge_properties) <- c("from", "to", "rel")
      }

      # Collect information into the 'edge_properties' data frame
      edge_properties[i, 1] <- edge_from[i]

      edge_properties[i, 2] <- edge_to[i]

      edge_properties[i, 3] <-
        ifelse(exists("relationship"),
               relationship[which((edge_from %in% edge_from[i]) &
                           (edge_to %in% edge_to[i]))],
               rep(NA, length(edge_from)))
    }

    return(edge_properties)
  }
}
