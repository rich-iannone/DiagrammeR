#' Create an edge data frame
#' @description Combine several named vectors for edges
#' and their attributes into a data frame, which can be
#' combined with other similarly-generated data frames,
#' or, added to a graph object.
#' @param from a vector of node ID values from which
#' edges are outbound. The vector length must equal
#' that of the \code{to} vector.
#' @param to a vector of node ID values to which edges
#' are incoming. The vector length must equal that of
#' the \code{from} vector.
#' @param rel an optional \code{rel} label for each
#' edge.
#' @param ... one or more named vectors for associated
#' attributes.
#' @return an edge data frame (edf).
#' @examples
#' # Create a simple edge data frame (edf) and
#' # view the results
#' edge_df <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "a")
#'
#' # Render the graph to make it viewable in
#' # the Viewer pane
#' render_graph(
#'   create_graph(edges_df = edge_df),
#'   output = "visNetwork")
#'
#' # Create an edge data frame with several
#' # additional parameters
#' edge_df <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "a",
#'     length = c(50, 100, 250),
#'     color = "green",
#'     width = c(1, 5, 2))
#'
#' # Render the graph to make it viewable in
#' # the Viewer pane
#' render_graph(
#'   create_graph(edges_df = edges),
#'   output = "visNetwork")
#' @export create_edge_df

create_edge_df <- function(from,
                           to,
                           rel = NULL,
                           ...) {

  # Stop function if vector lengths for `from` and
  # `to` are not equal
  stopifnot(length(from) == length(to))

  # Ensure that `from` and `to` vector are integers
  from <- as.integer(from)
  to <- as.integer(to)

  # if `rel` is NULL, create empty character vector;
  # class as character otherwise
  if (is.null(rel)) {
    rel <- as.character(rep("", length(from)))
  } else {
    rel <- as.character(rel)
  }

  if (!is.null(rel)) {
    # Expand vectors with single values to fill to
    # number of edges
    if (length(rel) == 1) {
      rel <- rep(rel, length(from))
    }

    # Expand vectors with `length` > `1` and
    # `length` < `length(from)`
    if (length(rel) > 1 &
        length(rel) < length(from)) {
      rel <-
        c(rel, rep("", (length(from) - length(rel))))
    }

    # Trim vectors with number of values exceeding
    # the number of edges
    if (length(rel) > length(from)) {
      rel <- rel[1:length(from)]
    }
  }

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {
    for (i in 1:length(extras)) {

      # Expand vectors with single values to fill to
      # the number of edges
      if (length(extras[[i]]) == 1) {
        extras[[i]] <- rep(extras[[i]], length(from))
      }

      # Expand vectors with `length` > `1` and
      # `length` < `length(from)`
      if (length(extras[[i]]) > 1 &
          length(extras[[i]]) < length(from)) {
        extras[[i]] <-
          c(extras[[i]],
            rep("", (length(from) -
                       length(extras[[i]]))))
      }

      # Trim vectors with number of values exceeding
      # the number of edges
      if (length(extras[[i]]) > length(from)) {
        extras[[i]] <- extras[[i]][1:length(from)]
      }
    }
  }

  if (inherits(extras, "tbl_df")) {
    edges_df <-
      dplyr::bind_cols(
        tibble::tibble(
          from = from,
          to = to,
          rel = rel),
        extras)
  } else {
    edges_df <-
      tibble::tibble(
        from = from,
        to = to,
        rel = rel)
  }

  return(edges_df)
}
