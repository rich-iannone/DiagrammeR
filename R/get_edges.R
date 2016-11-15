#' Get node IDs associated with edges
#' @description Obtain a vector, data frame, or list of
#' node IDs from a graph object or an edge data frame.
#' An optional filter by edge attribute can limit the
#' set of edges returned.
#' @param x either a graph object of class
#' \code{dgr_graph} or an edge data frame.
#' @param edge_attr an optional character vector of
#' edge attribute values for filtering the edges
#' returned.
#' @param match an option to provide a logical
#' expression with a comparison operator (\code{>},
#' \code{<}, \code{==}, or \code{!=}) followed by a
#' number for numerical filtering, or, a character
#' string for filtering the edges returned through
#' string matching.
#' @param return_type using \code{vector} (the
#' default), a vector of character objects
#' representing the edges is provided. With
#' \code{list} a list object will be provided that
#' contains vectors of outgoing and incoming node IDs
#' associated with edges. With \code{df}, a data frame
#' containing outgoing and incoming node IDs associated
#' with edges.
#' @return a list, data frame, or a vector object,
#' depending on the value given to \code{return_type}.
#' @examples
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_node_df(
#'     n = 4,
#'     type = "letter",
#'     color = c("red", "green", "grey", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to",
#'     color = c("pink", "blue", "red"),
#'     value = c(3.9, 2.5, 7.3))
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges)
#'
#' # Get all edges within a graph, returned as a list
#' get_edges(graph, return_type = "vector")
#' #> [1] "1 -> 4" "2 -> 3" "3 -> 1"
#'
#' # Get all edges within a graph, returned as a
#' # data frame
#' get_edges(graph, return_type = "df")
#' #>   from to
#' #> 1    1  4
#' #> 2    2  3
#' #> 3    3  1
#'
#' # Get all edges within a graph, returned as a vector
#' get_edges(graph, return_type = "list")
#' #> [[1]]
#' #> [1] "1" "2" "3"
#' #>
#' #> [[2]]
#' #> [1] "4" "3" "1"
#'
#' # Get a vector of edges using a numeric
#' # comparison (i.e., all edges with a `value`
#' # attribute greater than 3)
#' get_edges(
#'   graph,
#'   edge_attr = "value",
#'   match = "> 3",
#'   return_type = "vector")
#' #> [1] "1 -> 4" "3 -> 1"
#'
#' # Get a vector of edges using a match
#' get_edges(
#'   graph,
#'   edge_attr = "color",
#'   match = "pink",
#'   return_type = "vector")
#' #> [1] "1 -> 4"
#' @export get_edges

get_edges <- function(x,
                      edge_attr = NULL,
                      match = NULL,
                      return_type = "vector") {

  if (inherits(x, "dgr_graph")) {

    if (is_graph_empty(x) | nrow(x$edges_df) == 0) {

      return(NA)

    } else {

      edges_df <- x$edges_df
    }
  }

  if (inherits(x, "data.frame")) {

    if (colnames(x)[1] == "from" &
        colnames(x)[2] == "to") {

      edges_df <- x
    }
  }

  if (!is.null(edge_attr)) {
    if (length(edge_attr) > 1) {
      stop("Only one edge attribute can be specified.")
    }

    if (!(edge_attr %in% colnames(edges_df)[-(1:2)])) {
      stop("The specified attribute is not available.")
    }
  }

  if (is.null(edge_attr)) {
    from <- edges_df$from
    to <- edges_df$to
  }

  if (!is.null(edge_attr)) {

    # If a match term provided, filter using a logical
    # expression or a regex match
    if (!is.null(match)) {

      if (grepl("^>.*", match) | grepl("^<.*", match) |
          grepl("^==.*", match) | grepl("^!=.*", match)) {
        logical_expression <- TRUE } else {
          logical_expression <- FALSE
        }

      column_number <-
        which(colnames(edges_df) %in% edge_attr)

      if (logical_expression) {

        if (grepl("^>.*", match)) {
          rows_where_true_le <-
            which(edges_df[,column_number] >
                    as.numeric(gsub(">(.*)", "\\1", match)))
        }

        if (grepl("^>=.*", match)) {
          rows_where_true_le <-
            which(edges_df[,column_number] >=
                    as.numeric(gsub(">=(.*)", "\\1", match)))
        }

        if (grepl("^<.*", match)) {
          rows_where_true_le <-
            which(edges_df[,column_number] <
                    as.numeric(gsub("<(.*)", "\\1", match)))
        }

        if (grepl("^<=.*", match)) {
          rows_where_true_le <-
            which(edges_df[,column_number] <=
                    as.numeric(gsub("<=(.*)", "\\1", match)))
        }

        if (grepl("^==.*", match)) {
          rows_where_true_le <-
            which(edges_df[,column_number] ==
                    as.numeric(gsub("==(.*)", "\\1", match)))
        }

        from <- edges_df[rows_where_true_le, 1]
        to <- edges_df[rows_where_true_le, 2]
      }
    }

    # Filter using a `match` value
    if (logical_expression == FALSE) {

      if (is.numeric(match)) {
        match <- as.character(match)
      }

      rows_where_true_match <-
        which(match == as.character(edges_df[,column_number]))

      from <- edges_df[rows_where_true_match, 1]
      to <- edges_df[rows_where_true_match, 2]
    }
  }

  if (return_type == "list") {

    edges_list <- vector(mode = "list")
    edges_list[[1]] <- edges_list[[2]] <- vector(mode = "character")

    edges_list[[1]] <- c(edges_list[[1]], from)
    edges_list[[2]] <- c(edges_list[[2]], to)

    return(edges_list)
  }

  if (return_type == "df") {

    edges_list <- vector(mode = "list")
    edges_list[[1]] <- edges_list[[2]] <-
      vector(mode = "character")

    edges_list[[1]] <- c(edges_list[[1]], from)
    edges_list[[2]] <- c(edges_list[[2]], to)

    edges_df <-
      as.data.frame(edges_list,
                    stringsAsFactors = FALSE)

    colnames(edges_df) <- c("from", "to")

    edges_df[,1] <- as.integer(edges_df[,1])
    edges_df[,2] <- as.integer(edges_df[,2])

    return(edges_df)
  }

  if (return_type == "vector") {

    edges_list <- vector(mode = "list")
    edges_list[[1]] <- edges_list[[2]] <- vector(mode = "character")

    edges_list[[1]] <- c(edges_list[[1]], from)
    edges_list[[2]] <- c(edges_list[[2]], to)

    edges_vector <- paste(edges_list[[1]], "->", edges_list[[2]])

    return(edges_vector)
  }
}
