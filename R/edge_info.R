#' Get detailed information on edges
#' @description Obtain a data frame with
#' detailed information on edges and
#' their interrelationships within a graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a data frame containing information
#' specific to each edge within the graph.
#' @examples
#' # Set a seed
#' set.seed(24)
#'
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 26,
#'     label = TRUE,
#'     type = c(rep("a", 7),
#'              rep("b", 9),
#'              rep("c", 8),
#'              rep("d", 2)))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = sample(1:26, replace = TRUE),
#'     to = sample(1:26, replace = TRUE),
#'     rel = c(rep("rel_a", 7),
#'             rep("rel_b", 9),
#'             rep("rel_c", 8),
#'             rep("rel_d", 2)))
#'
#' # Create a graph using the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get a data frame with information about
#' # the graph's edges
#' edge_info(graph)
#' #>    from to   rel
#' #> 1     8  2 rel_a
#' #> 2     6 16 rel_a
#' #> 3    19 17 rel_a
#' #> 4    14  2 rel_a
#' #> 5    18  9 rel_a
#' #>..   ... ..   ...
#' @export edge_info

edge_info <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  if ("from" %in% colnames(graph$edges_df)) {
    edge_from <- graph$edges_df$from
  }

  if ("to" %in% colnames(graph$edges_df)) {
    edge_to <- graph$edges_df$to
  }

  if ("rel" %in% colnames(graph$edges_df)) {
    relationship <- graph$edges_df$rel
  }

  # For graphs with no edges, return NA
  if (nrow(graph$edges_df) == 0) {
    return(NA)
  }

  # For graphs with no edges, create an
  # `edge_properties` data frame
  if (!is.null(graph$edges_df)) {

    # Create data frame of edge properties
    for (i in 1:length(edge_from)) {

      if (i == 1) {
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
