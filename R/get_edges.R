#' Get node IDs associated with edges
#' @description Provides information on the node IDs associated with edges
#' from a graph object or an edge data frame.
#' @param ... a graph object or an edge data frame.
#' @param rel an optional character vector of \code{rel} values to filter the
#' output of node ID values.
#' @param return_type using \code{list} (the default) will provide a list
#' object containing vectors of outgoing and incoming node IDs associated
#' with edges. With \code{df}, a data frame containing outgoing and incoming
#' node IDs associated with edges. With \code{vector} or \code{string}, a
#' vector of character objects representing the edges is provided.
#' @return a list, data frame, or a vector object, depending on the value
#' given to \code{return_type}.
#' @examples
#' \dontrun{
#' # Before getting node ID values, create a simple graph
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
#' # Can get the 'outgoing' and 'incoming' node ID values
#' # in a list object
#' get_edges(graph, return_type = "list") # the default
#' #> [[1]]
#' #>  [1] "A" "H" "W" "U" "I" "M" "U" "T" "I" "R" "O"
#' #> [12] "G" "O" "A" "V" "I" "M" "K" "R" "T" "Y" "R"
#' #> [23] "M" "L" "H" "V"
#'
#' #> [[2]]
#' #>  [1] "Z" "U" "O" "K" "V" "M" "N" "C" "D" "Z" "B"
#' #> [12] "G" "U" "Y" "H" "V" "R" "V" "Z" "S" "Q" "I"
#' #> [23] "P" "S" "E" "P"
#'
#' # Similarly, you can specify that a data frame is given
#' get_edges(graph, return_type = "df")
#' #>    from to
#' #> 1     A  Z
#' #> 2     H  U
#' #> 3     W  O
#' #> 4     U  K
#' #> 5     I  V
#' #>..   ... ..
#'
#' # A character string with node IDs can instead be gotten
#' get_edges(graph, return_type = "vector")
#' #>  [1] "A -> Z" "H -> U" "W -> O" "U -> K" "I -> V"
#' #>  [6] "M -> M" "U -> N" "T -> C" "I -> D" "R -> Z"
#' #> [11] "O -> B" "G -> G" "O -> U" "A -> Y" "V -> H"
#' #> [16] "I -> V" "M -> R" "K -> V" "R -> Z" "T -> S"
#' #> [21] "Y -> Q" "R -> I" "M -> P" "L -> S" "H -> E"
#' #> [26] "V -> P"
#' }
#' @export get_edges

get_edges <- function(...,
                      rel = NULL,
                      return_type = "list"){

  objects <- list(...)

  object <- objects[[1]]

  if (class(object) == "dgr_graph"){

    if (is.null(object$edges_df)){
      return(NA)
    }

    object <- object$edges_df
  }

  if (return_type == "list"){

    edge_list <- vector(mode = "list")
    edge_list[[1]] <- edge_list[[2]] <- vector(mode = "character")

    if (is.null(rel)){
      edge_list[[1]] <- c(edge_list[[1]], object$from)
      edge_list[[2]] <- c(edge_list[[2]], object$to)
    }

    if (!is.null(rel)){
      edge_list[[1]] <- c(edge_list[[1]], object$from[which(object$rel %in% rel)])
      edge_list[[2]] <- c(edge_list[[2]], object$to[which(object$rel %in% rel)])
    }

    return(edge_list)
  }

  if (return_type == "df"){

    edge_list <- vector(mode = "list")
    edge_list[[1]] <- edge_list[[2]] <- vector(mode = "character")

    if (is.null(rel)){
      edge_list[[1]] <- c(edge_list[[1]], object$from)
      edge_list[[2]] <- c(edge_list[[2]], object$to)
    }

    if (!is.null(rel)){
      edge_list[[1]] <- c(edge_list[[1]], object$from[which(object$rel %in% rel)])
      edge_list[[2]] <- c(edge_list[[2]], object$to[which(object$rel %in% rel)])
    }

    edge_df <- as.data.frame(edge_list, stringsAsFactors = FALSE)
    colnames(edge_df) <- c("from", "to")

    return(edge_df)
  }

  if (return_type == "vector"){

    edge_list <- vector(mode = "list")
    edge_list[[1]] <- edge_list[[2]] <- vector(mode = "character")

    if (is.null(rel)){
      edge_list[[1]] <- c(edge_list[[1]], object$from)
      edge_list[[2]] <- c(edge_list[[2]], object$to)
    }

    if (!is.null(rel)){
      edge_list[[1]] <- c(edge_list[[1]], object$from[which(object$rel %in% rel)])
      edge_list[[2]] <- c(edge_list[[2]], object$to[which(object$rel %in% rel)])
    }

    edge_vector <- paste(edge_list[[1]], "->", edge_list[[2]])

    return(edge_vector)
  }
}
