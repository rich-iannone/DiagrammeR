#' Create a data frame with edges and their attributes
#' Combine several named vectors for edges and their attributes into a data frame, which can be combined with other similarly-generated data frame, or, added to a graph object.
#' @param ... one or more named vectors for edges and associated attributes.
#' @return a data frame.
#' @export create_edges

create_edges <- function(...){

  edges <- list(...)

  # Stop function if there are no list components
  stopifnot(!is.null(names(edges)))

  # Attempt to obtain the number of edges from the 'edge_from' column
  # If 'edge_from' column exists, ensure that it is classed as character
  if ("edge_from" %in% names(edges)){
    number_of_edges_from <- length(edges$edge_from)
    edges$edge_from <- as.character(edges$edge_from)
  }

  # Attempt to obtain the number of edges from the 'from' column
  # If 'from' column exists, ensure that it is classed as character
  if ("from" %in% names(edges)){
    number_of_edges_from <- length(edges$from)
    edges$from <- as.character(edges$from)
  }

  # Attempt to obtain the number of edges from the 'edge_to' column
  # If 'edge_to' column exists, ensure that it is classed as character
  if ("edge_to" %in% names(edges)){
    number_of_edges_to <- length(edges$edge_to)
    edges$edge_to <- as.character(edges$edge_to)
  }

  # Attempt to obtain the number of edges from the 'to' column
  # If 'to' column exists, ensure that it is classed as character
  if ("to" %in% names(edges)){
    number_of_edges_to <- length(edges$to)
    edges$to <- as.character(edges$to)
  }

  stopifnot(number_of_edges_from == number_of_edges_to)

  number_of_edges <- number_of_edges_from

  for (i in 1:length(edges)){

    # Expand vectors with single values to fill to number of edges
    if (length(edges[[i]]) == 1){
      edges[[i]] <- rep(edges[[i]], number_of_edges)
    }

    # Expand vectors with length > 1 and length < 'number_of_edges'
    if (length(edges[[i]]) > 1 & length(edges[[i]]) < number_of_edges){
      edges[[i]] <- c(edges[[i]], rep("", (number_of_edges - length(edges[[i]]))))
    }

    # Trim vectors with number of values exceeding number of edges
    if (length(edges[[i]]) > number_of_edges){
      edges[[i]] <- edges[[i]][1:number_of_edges]
    }
  }

  edges_df <- as.data.frame(edges, stringsAsFactors = FALSE)

  return(edges_df)
}
