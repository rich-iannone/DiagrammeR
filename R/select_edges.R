#' Select edges
#' @description Select edges from a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param from an optional vector of node IDs from which the edge is
#' outgoing for filtering the list of edges present in the graph.
#' @param to an optional vector of node IDs to which the edge is
#' incoming for filtering the list of edges present in the graph.
#' @param edge_attr an optional character vector of edge attribute values for
#' filtering the edges returned.
#' @param comparison an optional logical expression for filtering the edges
#' returned.
#' @param regex an optional regular expression (regex) for filtering the
#' edges returned.
#' @param set_op an option to perform a set union, intersection, or
#' difference on the previous selection, if it exists.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_edges

select_edges <- function(graph,
                         from = NULL,
                         to = NULL,
                         edge_attr = NULL,
                         comparison = NULL,
                         regex = NULL,
                         set_op = "union"){

  if (!is.null(comparison) & !is.null(regex)){
    stop("A comparison and a regex pattern cannot be used together.")
  }

  if (is_graph_empty(graph)){
    stop("The graph is empty so no selections can be made.")
  }

  if (edge_count(graph) == 0){
    stop("The graph has no edges so no selections can be made.")
  }

  edges_df <- graph$edges_df

  if (!is.null(edge_attr)){
    if (length(edge_attr) > 1){
      stop("Only one edge attribute can be specified.")
    }

    if (!(edge_attr %in% colnames(edges_df)[-(1:2)])){
      stop("The specified attribute is not availalbe")
    }
  }

  if (is.null(edge_attr)){

    if (is.null(from) & !is.null(to)){

      if (any(!(to %in% edges_df$to))){
        stop("One of more of the incoming nodes specified are not part of an edge.")
      }

      edges_df <-
        edges_df[which(edges_df$to %in% to),]

    } else if (!is.null(from) & is.null(to)){

      if (any(!(from %in% edges_df$from))){
        stop("One of more of the outgoing nodes specified are not part of an edge.")
      }

      edges_df <-
        edges_df[which(edges_df$from %in% from),]

    } else if (is.null(from) & is.null(to)){

      if (any(!(from %in% edges_df$from))){
        stop("One of more of the outgoing nodes specified are not part of an edge.")
      }

      if (any(!(to %in% edges_df$to))){
        stop("One of more of the incoming nodes specified are not part of an edge.")
      }

      edges_df <- edges_df

    } else {

      edges_df <-
        edges_df[which((edges_df$from %in% from) &
                         (edges_df$to %in% to)),]
    }

    from_selected <- edges_df$from
    to_selected <- edges_df$to
  }

  if (!is.null(edge_attr)){

    column_number <-
      which(colnames(edges_df) %in% edge_attr)

    # Filter using a logical expression
    if (!is.null(comparison) & is.null(regex)){

      if (grepl("^>.*", comparison)){
        rows_where_true_le <-
          which(edges_df[,column_number] >
                  as.numeric(gsub(">(.*)", "\\1", comparison)))
      }

      if (grepl("^>=.*", comparison)){
        rows_where_true_le <-
          which(edges_df[,column_number] >=
                  as.numeric(gsub(">=(.*)", "\\1", comparison)))
      }

      if (grepl("^<.*", comparison)){
        rows_where_true_le <-
          which(edges_df[,column_number] <
                  as.numeric(gsub("<(.*)", "\\1", comparison)))
      }

      if (grepl("^<=.*", comparison)){
        rows_where_true_le <-
          which(edges_df[,column_number] <=
                  as.numeric(gsub("<=(.*)", "\\1", comparison)))
      }

      if (grepl("^==.*", comparison)){
        rows_where_true_le <-
          which(edges_df[,column_number] ==
                  as.numeric(gsub("==(.*)", "\\1", comparison)))
      }

      from_selected <- edges_df[rows_where_true_le, 1]
      to_selected <- edges_df[rows_where_true_le, 2]
    }

    # Filter using a regex
    if (is.null(comparison) & !is.null(regex)){

      rows_where_true_regex <-
        which(grepl(regex, as.character(edges_df[,column_number])))

      from_selected <- edges_df[rows_where_true_regex, 1]
      to_selected <- edges_df[rows_where_true_regex, 2]
    }
  }

  # Obtain vectors of node IDs associated with edges already present
  if (!is.null(graph$selection)){
    if (!is.null(graph$selection$edges)){
      from_prev_selection <- graph$selection$edges$from
      to_prev_selection <- graph$selection$edges$to
    }
  } else {
    from_prev_selection <- vector(mode = "character")
    to_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected edges into graph's selection section
  if (set_op == "union"){

    from_combined <- union(from_prev_selection, from_selected)
    to_combined <- union(to_prev_selection, to_selected)
  }

  graph$selection$edges$from <- from_combined
  graph$selection$edges$to <- to_combined

  return(graph)
}
