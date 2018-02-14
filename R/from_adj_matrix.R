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
#' @param graph_name an optional string for labeling
#' the graph object.
#' @param write_backups an option to write incremental
#' backups of changing graph states to disk. If
#' \code{TRUE}, a subdirectory of the working directory
#' will be used to store \code{RDS} files. The
#' default value is \code{FALSE} so one has to opt in
#' to use this functionality.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an adjacency matrix
#' adj_matrix <-
#'   sample(0:1, 100,
#'          replace = TRUE,
#'          prob = c(0.9,0.1)) %>%
#'   matrix(nc = 10)
#'
#' # Create a graph from the adjacency matrix
#' graph <-
#'   from_adj_matrix(adj_matrix)
#' @importFrom igraph graph_from_adjacency_matrix
#' @export from_adj_matrix

from_adj_matrix <- function(x,
                            mode = "undirected",
                            weighted = FALSE,
                            use_diag = TRUE,
                            graph_name = NULL,
                            write_backups = FALSE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Stop function if x is not a matrix object
  if (!inherits(x, "matrix")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The input for this function must be a matrix object")
  }

  # Stop function if the matrix is not a square matrix
  if (ncol(x) != nrow(x)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The input matrix must be a square matrix")
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
  graph <-
    from_igraph(
      igraph,
      graph_name = graph_name,
      write_backups = write_backups)

  # Add edge ID values to `graph`
  if (nrow(graph$edges_df) > 0) {
    graph$edges_df$id <- as.integer(1:nrow(graph$edges_df))
  }

  graph
}
