#' Create, read, update, delete, or report status of an edge relationship
#' @description From a graph object of class \code{dgr_graph}, query an edge
#' in the graph (defined by a pair of node IDs extant in the graph) and perform
#'  operations on the relationship for that edge.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#'  \code{create_graph}.
#' @param from a node ID from which the edge to be queried is outgoing.
#' @param to a node ID to which the edge to be queried is incoming.
#' @param action the operation to perform on the edge's relationship attribute.
#' To remove a relationship from an edge, use either \code{delete},
#' \code{remove}, or \code{drop}. To add a relationship to an edge with no
#' set relationship, use \code{add} or \code{create}. To update an edge
#' relationship, use \code{update}. To return the value of an edge
#' relationship, use \code{read}. To determine whether there is a set
#' relationship, use \code{check}.
#' @param value a string denoting the relationship, to be supplied when either
#' adding or updating an edge relationship.
#' @return a graph object of class \code{dgr_graph}.
#' @export edge_rel

edge_rel <- function(graph,
                     from,
                     to,
                     action = "read",
                     value = NULL){

  # Determine if edge is present within the graph
  edge_is_in_graph <- edge_present(graph = graph, from = from, to = to)

  # Stop function if edge is not present within the graph
  if (edge_is_in_graph == FALSE){

    stop("The specified edge is not present in the graph.")
  }

  if (edge_is_in_graph == TRUE){

    edge_row <- which(graph$edges_df$from == from & graph$edges_df$to == to)

    relationship_set <-
      ifelse(is.null(graph$edges_df$rel[edge_row]) ||
               graph$edges_df$rel[edge_row] == "",
             FALSE, TRUE)

    # Remove relationship if a relationship is set
    if (action %in% c("delete", "remove", "drop")){

      if (relationship_set == FALSE){

        return(graph)
      }

      if (relationship_set == TRUE){

        graph$edges_df$rel[edge_row] <- ""

        return(graph)
      }
    }

    # Add a relationship to an edge with no set relationship
    if (action %in% c("add", "create")){

      if (relationship_set == TRUE){

        return(graph)
      }

      if (relationship_set == FALSE & !is.null(value)){

        if (is.null(graph$edges_df$rel)){

          rel_col <-
            vector(mode = "character",
                 length = nrow(graph$edges_df))

          rel_col[edge_row] <- value

          graph$edges_df$rel <- rel_col
        }

        if (!is.null(graph$edges_df$rel)){

          graph$edges_df$rel[edge_row] <- value
        }

        return(graph)
      }
    }

    # Update an existing relationship for an edge
    if (action == "update"){

      if (relationship_set == FALSE){

        return(graph)
      }

      if (relationship_set == TRUE & !is.null(value)){

        graph$edges_df$rel[edge_row] <- value

        return(graph)
      }
    }

    # Return the value of an existing relationship for an edge
    if (action == "read"){

      if (relationship_set == FALSE){

        relationship_value <- NA

        return(relationship_value)
      }

      if (relationship_set == TRUE){

        relationship_value <- graph$edges_df$rel[edge_row]

        return(relationship_value)
      }
    }

    # Determine whether a relationship has been set
    if (action == "check"){

      if (relationship_set == FALSE){

        return(FALSE)
      }

      if (relationship_set == TRUE){

        return(TRUE)
      }
    }
  }
}
