#' Create a data frame with edges and their attributes
#' @description Combine several named vectors for edges and their attributes
#' into a data frame, which can be combined with other similarly-generated data
#' frame, or, added to a graph object.
#' @param from a vector of node ID values from which edges are outbound. The
#' vector length must equal to that of the \code{to} vector.
#' @param to a vector of node ID values to which edges are incoming. The
#' vector length must equal to that of the \code{from} vector.
#' @param rel an optional 'rel' description for each edge.
#' @param ... one or more named vectors for associated attributes.
#' @return a data frame.
#' @examples
#' \dontrun{
#' # Create a simple edge data frame and view results
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to")
#'
#' render_graph(create_graph(edges_df = edges),
#'                           output = "visNetwork")
#'
#' # Create an edge data frame with additional parameters
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to",
#'                length = c(50, 100, 250),
#'                color = "green",
#'                width = c(1, 1, 2))
#'
#' render_graph(create_graph(edges_df = edges),
#'                           output = "visNetwork")
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
