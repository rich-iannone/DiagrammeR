#' Get count of all edges or edges with distinct
#' relationship types
#' @description From a graph object of class
#' \code{dgr_graph}, get a count of edges in the graph
#' and optionally obtain a count of edges by their
#' relationship type.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param rel either a logical value, where \code{TRUE}
#' provides a named vector of edge count by type and
#' \code{FALSE} (the default) provides a total count of
#' edges, or, a string corresponding to one or more
#' edge relationship types.
#' @return a numeric vector of single length.
#' @examples
#' # Set a seed
#' set.seed(23)
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
#' # Get a total count of edges in the graph
#' edge_count(graph, rel = FALSE)
#' #> [1] 26
#'
#' # Get a count of edges that have the
#' # relationship of `rel_a`
#' edge_count(graph, rel = "rel_a")
#' #> [1] 7
#'
#' # Get a count of edges with relationships
#' # `rel_a` and `rel_b`
#' edge_count(graph, rel = c("rel_a", "rel_b"))
#' #> [1] 16
#' @export edge_count

edge_count <- function(graph,
                       rel = FALSE) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # If graph is empty, return 0
  if (is_graph_empty(graph)) {
    return(0)
  }

  # If `rel` is a string, get a count of edges for
  # a specific rel
  if (inherits(rel, "character")) {
    count_of_rel <-
      length(which(graph$edges_df$rel %in% rel))
    return(count_of_rel)
  }

  # If `rel` is set to FALSE, get a total count of edges
  if (all(inherits(rel, "logical") &
          rel == FALSE)) {
    total_edge_count <- nrow(graph$edges_df)
    return(total_edge_count)
  }

  # If `rel` set to TRUE, get a named vector of counts
  # by relationship
  if (all(inherits(rel, "logical") & rel)) {

    all_relationships <- unique(graph$edges_df$rel)

    if (any(is.na(all_relationships))) {
      all_relationships[
        which(is.na(all_relationships))] <- ""
    }

    for (i in 1:length(all_relationships)) {
      if (i == 1) {
        total_edge_count <- vector(mode = "numeric")
      }

      total_edge_count <-
        c(total_edge_count,
          nrow(graph$edges_df[
            which(graph$edges_df$rel ==
                    all_relationships[i]),]))

      if (i == length(all_relationships)) {
        names(total_edge_count) <- all_relationships

        if (any(names(total_edge_count) == "")) {
          names(total_edge_count)[
            which(names(total_edge_count) == "")] <-
            "<no rel>"

          total_edge_count <-
            c(total_edge_count[
              which(names(total_edge_count) == "<no rel>")],
              total_edge_count[
                -which(names(total_edge_count) == "<no rel>")])
        }
      }
    }
    return(total_edge_count)
  }
}
