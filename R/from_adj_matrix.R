#' Create a graph using an adjacency matrix
#' @description Using an adjacency matrix object,
#' generate a graph of class \code{dgr_graph}.
#' @param x a square \code{matrix} object serving as
#' the adjacency matrix.
#' @param mode the method in which to interpret the
#' input adjacency matrix. Options include:
#' \code{undirected}, \code{directed}, \code{upper},
#' \code{lower}, \code{max}, \code{min}, and
#' \code{plus}.
#' @param weighted whether to create a weighted graph
#' from the adjacency matrix.
#' @param use_diag whether to use the diagonal of the
#' adjacency matrix in calculations. If \code{TRUE}
#' then the diagonal values will be included as is. If
#' \code{FALSE} then the diagonal values will be
#' replaced with zero values before inclusion in any
#' calculations.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an adjacency matrix
#' adj_matrix <-
#'   sample(0:1, 100,
#'          replace = TRUE,
#'          prob = c(0.9,0.1)) %>%
#'   matrix(nc = 10)
#'
#' graph <- from_adj_matrix(adj_matrix)
#' @importFrom igraph graph_from_adjacency_matrix
#' @export from_adj_matrix

from_adj_matrix <- function(x,
                            mode = "undirected",
                            weighted = FALSE,
                            use_diag = TRUE) {

  # Stop function if x is not a matrix object
  if (!inherits(x, "matrix")) {
    stop("The input for this function must be a matrix object.")
  }

  # Stop function if the matrix is not a square matrix
  if (ncol(x) != nrow(x)) {
    stop("The input matrix must be a square matrix.")
  }

  # If FALSE provided for `weighted`, change value to
  # NULL for `graph_from_adjacency_matrix()`
  if (weighted == FALSE) {
    weighted <- NULL
  }

  # Generate an igraph graph from the adjacency matrix
  if (is.null(colnames(x))) {

    igraph <-
      igraph::graph_from_adjacency_matrix(
        adjmatrix = x,
        mode = mode,
        weighted = weighted,
        diag = use_diag)

  } else if (!is.null(colnames(x))) {

    igraph <-
      igraph::graph_from_adjacency_matrix(
        adjmatrix = x,
        mode = mode,
        weighted = weighted,
        diag = use_diag,
        add.colnames = "label")
  }

  # Generate the graph object from an igraph graph
  graph <- from_igraph(igraph)

  return(graph)
}
