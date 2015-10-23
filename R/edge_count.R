#' Get count of all edges or edges with distinct relationship types
#' @description From a graph object of class \code{dgr_graph}, get a count of
#' edges in the graph and optionally obtain a count of edges by their
#' relationship type.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param rel either a logical value, where \code{TRUE} provides a
#' named vector of edge count by type and \code{FALSE} (the default) provides
#' a total count of edges, or, a string corresponding to one or more edge
#' relationship types.
#' @return a numeric vector of single length.
#' @examples
#' \dontrun{
#' # Before getting counts of edges, create a simple graph
#' nodes <-
#'   create_nodes(nodes = LETTERS,
#'                label = TRUE,
#'                type = c(rep("a_to_g", 7),
#'                         rep("h_to_p", 9),
#'                         rep("q_to_x", 8),
#'                         rep("y_and_z", 2)))
#'
#' edges <-
#'   create_edges(from = sample(LETTERS, replace = TRUE),
#'                to = sample(LETTERS, replace = TRUE),
#'                rel = c(rep("rel_a", 7),
#'                        rep("rel_b", 9),
#'                        rep("rel_c", 8),
#'                        rep("rel_d", 2)))
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges,
#'                graph_attrs = "layout = neato",
#'                node_attrs = c("fontname = Helvetica",
#'                               "shape = circle"))
#'
#' # Get a total count of edges with no grouping
#' edge_count(graph, rel = FALSE)
#' #> [1] 26
#'
#' # Get a count of edge of one or more specified rels
#' edge_count(graph, rel = "rel_a")
#' #> [1] 7
#'
#' edge_count(graph, rel = c("rel_a", "rel_b"))
#' #> [1] 16
#' }
#' @export edge_count

edge_count <- function(graph,
                       rel = FALSE){

  # If graph is empty, return 0
  if (is_graph_empty(graph) == TRUE){

    return(0)
  }

  # If rel is a string, get a count of edge for a specific rel
  if (class(rel) == "character"){

    count_of_rel <-
      length(which(graph$edges_df$rel %in% rel))

    return(count_of_rel)
  }

  # If rel is set to FALSE, get a total count of edges
  if (all(class(rel) == "logical" & rel == FALSE)){

    total_edge_count <- length(graph$edges_df$rel)

    return(total_edge_count)
  }

  # If rel set to TRUE, get a named vector of counts by relationship
  if (all(class(rel) == "logical" & rel == TRUE)){

    all_relationships <- unique(graph$edges_df$rel)

    if (any(is.na(all_relationships))){

      all_relationships[which(is.na(all_relationships))] <- ""
    }

    for (i in 1:length(all_relationships)){

      if (i == 1) total_edge_count <- vector(mode = "numeric")

      total_edge_count <-
        c(total_edge_count,
          nrow(graph$edges_df[which(graph$edges_df$rel ==
                                      all_relationships[i]),]))

      if (i == length(all_relationships)){
        names(total_edge_count) <- all_relationships

        if (any(names(total_edge_count) == "")){
          names(total_edge_count)[which(names(total_edge_count) == "")] <-
            "<no rel>"

          total_edge_count <-
            c(total_edge_count[which(names(total_edge_count) == "<no rel>")],
              total_edge_count[-which(names(total_edge_count) == "<no rel>")])
        }
      }
    }

    return(total_edge_count)
  }
}
