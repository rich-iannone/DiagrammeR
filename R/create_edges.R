#' Create a data frame with edges and their attributes
#' @description Combine several named vectors for edges and their attributes
#' into a data frame, which can be combined with other similarly-generated data
#' frame, or, added to a graph object.
#' @param ... one or more named vectors for edges and associated attributes;
#' the names for the named vectors must include \code{from} and \code{to}
#' alongside any named vectors for the edge attributes and ancillary data.
#' @return a data frame.
#' @examples
#' \dontrun{
#' # Create an edge data frame
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to")
#' }
#' @export create_edges

create_edges <- function(from,
                         to,
                         rel = NULL,
                         ...){

  # Stop function if vector lengths for 'from' and 'to' not equal
  stopifnot(length(from) == length(to))

  # Ensure that 'from' and 'to' vectors classed as character
  from <- as.character(from)
  to <- as.character(to)

  # rel is NULL, create empty character vector; class as character
  # otherwise
  if (is.null(rel)){
    rel <- as.character(rep("", length(from)))
  } else {
    rel <- as.character(rel)
  }

  if (!is.null(rel)){

    # Expand vectors with single values to fill to number of edges
    if (length(rel) == 1){
      rel <- rep(rel, length(from))
    }

    # Expand vectors with length > 1 and length < 'length(from)'
    if (length(rel) > 1 & length(rel) < length(from)){
      rel <-
        c(rel, rep("", (length(from) - length(rel))))
    }

    # Trim vectors with number of values exceeding number of edges
    if (length(rel) > length(from)){
      rel <- rel[1:length(from)]
    }
  }

  # Collect extra vectors of data as 'extras'
  extras <- list(...)

  if (length(extras) > 0){

    for (i in 1:length(extras)){

      # Expand vectors with single values to fill to number of edges
      if (length(extras[[i]]) == 1){
        extras[[i]] <- rep(extras[[i]], length(from))
      }

      # Expand vectors with length > 1 and length < length(from)
      if (length(extras[[i]]) > 1 & length(extras[[i]]) < length(from)){
        extras[[i]] <- c(extras[[i]],
                         rep("", (length(from) - length(extras[[i]]))))
      }

      # Trim vectors with number of values exceeding number of edges
      if (length(extras[[i]]) > length(from)){
        extras[[i]] <- extras[[i]][1:length(from)]
      }
    }
  }

  if (length(extras) > 0){

    edges_df <- data.frame(from = from, to = to, rel = rel,
                           extras, stringsAsFactors = FALSE)
  } else {

    edges_df <- data.frame(from = from, to = to, rel = rel,
                           stringsAsFactors = FALSE)
  }

  return(edges_df)
}
