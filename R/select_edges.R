#' Select edges in a graph
#' @description Select edges from a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param edge_attr an optional character vector of edge attribute values for
#' filtering the edges returned.
#' @param search an option to provide a logical expression with a comparison
#' operator (\code{>}, \code{<}, \code{==}, or \code{!=}) followed by a number
#' for numerical filtering, or, a regular expression for filtering the nodes
#' returned through string matching.
#' @param set_op the set operation to perform upon consecutive selections
#' of graph edges This can either be as a \code{union} (the default), as an
#' \code{intersection}, or, as a \code{difference} on the previous selection,
#' if it exists.
#' @param from an optional vector of node IDs from which the edge is
#' outgoing for filtering the list of edges present in the graph.
#' @param to an optional vector of node IDs to which the edge is
#' incoming for filtering the list of edges present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_edges

select_edges <- function(graph,
                         edge_attr = NULL,
                         search = NULL,
                         set_op = "union",
                         from = NULL,
                         to = NULL){

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
      stop("The specified attribute is not available.")
    }
  }

  if (is.null(edge_attr)){

    if (is.null(from) & !is.null(to)){

      if (any(!(to %in% edges_df$to))){
        stop("One of more of the incoming nodes specified are not part of an edge.")
      }

      edges_selected <-
        get_edges(edges_df[which(edges_df$to %in% to),],
                  return_type = "vector")

    } else if (!is.null(from) & is.null(to)){

      if (any(!(from %in% edges_df$from))){
        stop("One of more of the outgoing nodes specified are not part of an edge.")
      }

      edges_selected <-
        get_edges(edges_df[which(edges_df$from %in% from),],
                  return_type = "vector")

    } else if (is.null(from) & is.null(to)){

      edges_selected <-
        get_edges(edges_df, return_type = "vector")

    } else {

      edges_selected <-
        get_edges(edges_df[which((edges_df$from %in% from) &
                                   (edges_df$to %in% to)),],
                  return_type = "vector")
    }
  }

  if (!is.null(edge_attr)){

    column_number <-
      which(colnames(edges_df) %in% edge_attr)

    # If a search term provided, filter using a logical expression
    # or a regex match
    if (!is.null(search)){

      if (grepl("^>.*", search) | grepl("^<.*", search) |
          grepl("^==.*", search) | grepl("^!=.*", search)){
        logical_expression <- TRUE } else {
          logical_expression <- FALSE
        }

      # Filter using a logical expression
      if (logical_expression){

        if (grepl("^>.*", search)){
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) >
                    as.numeric(gsub(">(.*)", "\\1", search)))
        }


        if (grepl("^<.*", search)){
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) <
                    as.numeric(gsub("<(.*)", "\\1", search)))
        }


        if (grepl("^==.*", search)){
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) ==
                    as.numeric(gsub("==(.*)", "\\1", search)))
        }

        if (grepl("^!=.*", search)){
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) !=
                    as.numeric(gsub("!=(.*)", "\\1", search)))
        }

        edges_selected <-
          get_edges(edges_df[rows_where_true_le, ],
                    return_type = "vector")
      }

      # Filter using a `search` value as a regular expression
      if (logical_expression == FALSE){

        rows_where_true_regex <-
          which(grepl(search, as.character(edges_df[,column_number])))

        edges_selected <-
          get_edges(edges_df[rows_where_true_regex, ],
                    return_type = "vector")
      }
    }
  }

  # Obtain vectors of node IDs associated with edges already present
  if (!is.null(graph$selection)){
    if (!is.null(graph$selection$edges)){
      from_prev_selection <- graph$selection$edges$from
      to_prev_selection <- graph$selection$edges$to

      edges_prev_selection <-
        sapply(1:length(from_prev_selection),
               function(x) paste(from_prev_selection[x],
                                 "->",
                                 to_prev_selection[x]))
    }
  } else {
    edges_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected edges into graph's selection section
  if (set_op == "union"){
    edges_combined <- union(edges_prev_selection, edges_selected)
  } else if (set_op == "intersect"){
    edges_combined <- intersect(edges_prev_selection, edges_selected)
  } else if (set_op == "difference"){
    edges_combined <- setdiff(edges_prev_selection, edges_selected)
  }

  from_combined <- gsub("\\s", "",
                        gsub("(.*)(->|--)(.*)", "\\1", edges_combined))

  to_combined <- gsub("\\s", "",
                      gsub("(.*)(->|--)(.*)", "\\3", edges_combined))

  # Create selection of edges
  graph$selection$edges$from <- from_combined
  graph$selection$edges$to <- to_combined

  # Remove any selection of nodes
  graph$selection$nodes <- NULL

  return(graph)
}
