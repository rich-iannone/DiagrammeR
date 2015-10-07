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
#' # Get a total count of edges with no grouping
#' edge_count(graph, rel = FALSE)
#' #> [1] 26
#' }
#' @export edge_count

edge_count <- function(graph,
                       rel = FALSE){

  # If type is FALSE, get a total count of edges
  if (rel == FALSE){

    if (is_graph_empty(graph) == TRUE){

      total_edge_count <- 0
    }

    if (is_graph_empty(graph) == FALSE){

      total_edge_count <- length(get_edges(graph)[[1]])
    }

    return(total_edge_count)
  }

  # If relationship set to TRUE, get a named vector of counts by relationship
  if (rel == TRUE){

    if (is_graph_empty(graph) == TRUE){

      total_edge_count <- 0
    }

    if (is_graph_empty(graph) == FALSE){

      for (i in 1:length(get_edges(graph)[[1]])){

        if (i == 1){
          all_edges <- get_edges(graph, return_type = "df")
          all_relationships <- vector(mode = "character")
        }

        all_relationships <-
          c(all_relationships,
            edge_rel(graph = graph,
                     from = all_edges[i,1],
                     to = all_edges[i,2],
                     action = "read"))

        all_relationships <- unique(all_relationships)

        if (any(is.na(all_relationships))){

          all_relationships[which(is.na(all_relationships))] <- ""
        }
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
    }

    return(total_edge_count)
  }

  # If relationship is a character vector, get counts by supplied relationships
  if (class(rel) == "character"){

    if (is_graph_empty(graph) == TRUE){

      total_edge_count <- 0
    }

    if (is_graph_empty(graph) == FALSE){

      # Determine all edge relationship types available in graph
      for (i in 1:length(get_edges(graph)[[1]])){

        if (i == 1){
          all_edges <- get_edges(graph, return_type = "df")
          all_relationships <- vector(mode = "character")
        }

        all_relationships <-
          c(all_relationships,
            edge_rel(graph = graph,
                     from = all_edges[i,1],
                     to = all_edges[i,2],
                     action = "read"))

        all_relationships <- unique(all_relationships)

        if (any(is.na(all_relationships))){

          all_relationships[which(is.na(all_relationships))] <- ""
        }
      }

      # Determine whether those edge relationships provides are all
      # available in the graph
      relationships_are_available <-
        ifelse(all(rel %in% all_relationships), TRUE, FALSE)

      if (relationships_are_available){

        # Determine how many relationship rows are unsets for the edges
        # available in the graph
        unset_relationship_for_node_count <-
          nrow(subset(graph$edges_df, rel == ''))

        # Determine the total count of edges with relationship not set
        # (if that was specified)
        if (rel == ""){

          total_edge_count <- unset_relationship_for_node_count
        }

        # Determine the total count of edges with relationship set (if
        # that was specified)
        if (rel != ""){

          if (all(graph$edges_df$rel == '') == FALSE){

            edges_df_set_relationship <- graph$edges_df

          } else {

            edges_df_set_relationship <-
              graph$edges_df[-which(graph$edges_df$rel == ''),]
          }

          for (i in 1:length(rel)){

            if (i == 1) total_edge_count <- vector(mode = "numeric")

            total_edge_count <-
              c(total_edge_count,
                nrow(edges_df_set_relationship[which(edges_df_set_relationship$rel ==
                                                       rel[i]),]))

            if (i == length(rel)){

              names(total_edge_count) <- rel
            }
          }
        }

        if (relationships_are_available == FALSE){

          total_edge_count <- NA
        }
      }
    }

    return(total_edge_count)
  }
}
