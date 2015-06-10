#' Get node IDs associated with edges
#' @description Provides information on the node IDs associated with edges from one or more edge data frames, or, a graph object.
#' @param ... a collection of edge data frames or graph objects.
#' @param return_type using \code{list} (the default) will provide a list object containing vectors of outgoing and incoming node IDs associated with edges. With \code{df}, a data frame containing outgoing and incoming node IDs associated with edges. With \code{vector} or \code{string}, a vector of character objects representing the edges is provided.
#' @return a list, data frame, or a vector object, depending on the value given to \code{return_type}.
#' @examples
#' \dontrun{
#' # Before getting node ID values, create a simple graph
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
#'                relationship = "letter_to_letter")
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
                      return_type = "list"){

  objects <- list(...)

  for (i in 1:length(objects)){

    if (i == 1) {
      edge_list <- vector(mode = "list")
      edge_list[[1]] <- edge_list[[2]] <- vector(mode = "character")
    }

    object <- objects[[i]]

    if (class(object) == "dgr_graph"){

      object_type <- "dgr_graph"
    }

    if (any(c("from", "to") %in% colnames(object))){

      object_type <- "edge_df"
    }
  }

  if (object_type == "dgr_graph"){

    object <- object$edges_df

    no_edges <- FALSE

    if ("from" %in% colnames(object)){

      from_column <- which(colnames(object) == "from")

    } else {

      no_edges <- TRUE
    }

    if ("to" %in% colnames(object)){

      to_column <- which(colnames(object) == "to")

    } else {

      no_edges <- TRUE
    }

    if (return_type == "list" & no_edges == TRUE){

      edge_list[[1]] <- edge_list[[2]] <- NA

      return(edge_list)
    }

    if (return_type == "df" & no_edges == TRUE){

      edge_df <- as.data.frame(edge_list)
      colnames(edge_df) <- c("from", "to")

      return(edge_df)
    }

    if (return_type %in% c("vector", "string") & no_edges == TRUE){

      edge_vector <- NA

      return(edge_vector)
    }

    edge_list[[1]] <- c(edge_list[[1]], object[,from_column])
    edge_list[[2]] <- c(edge_list[[2]], object[,to_column])
  }

  if (object_type == "edge_df"){

    both_from_to_columns <- all(c(any(c("from") %in%
                                        colnames(object))),
                                any(c("to") %in%
                                      colnames(object)))

    if (exists("both_from_to_columns")){

      if (both_from_to_columns == TRUE){

        from_column <- which(colnames(object) %in% "from")[1]

        to_column <- which(colnames(object) %in% "to")[1]
      }
    }

    edge_list[[1]] <- c(edge_list[[1]], object[,from_column])
    edge_list[[2]] <- c(edge_list[[2]], object[,to_column])
  }

  if (return_type == "list"){

    return(edge_list)
  }

  if (return_type == "df"){

    edge_df <- as.data.frame(edge_list)
    colnames(edge_df) <- c("from", "to")

    return(edge_df)
  }

  if (return_type %in% c("vector", "string")){

    edge_vector <- paste(edge_list[[1]], "->", edge_list[[2]])

    return(edge_vector)
  }
}
