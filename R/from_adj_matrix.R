#' Create a graph using an adjacency matrix
#'
#' @description
#'
#' Using an adjacency matrix object, generate a graph of class `dgr_graph`.
#'
#' @param x A square `matrix` object serving as the adjacency matrix.
#' @param mode The method in which to interpret the input adjacency matrix.
#'   Options include: `undirected`, `directed`, `upper`, `lower`, `max`, `min`,
#'   and `plus`.
#' @param weighted Whether to create a weighted graph from the adjacency matrix.
#' @param use_diag Whether to use the diagonal of the adjacency matrix in
#'   calculations. If `TRUE` then the diagonal values will be included as is. If
#'   `FALSE` then the diagonal values will be replaced with zero values before
#'   inclusion in any calculations.
#' @inheritParams create_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Create an adjacency matrix
#' adj_matrix <-
#'   sample(
#'     0:1, 100,
#'     replace = TRUE,
#'     prob = c(0.9,0.1)
#'   ) %>%
#'   matrix(ncol = 10)
#'
#' # Create a graph from the adjacency matrix
#' graph <- from_adj_matrix(adj_matrix)
#'
#' @export
from_adj_matrix <- function(
    x,
    mode = "undirected",
    weighted = FALSE,
    use_diag = TRUE,
    graph_name = NULL,
    write_backups = FALSE,
    display_msgs = FALSE
) {

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
      write_backups = write_backups,
      display_msgs = display_msgs
    )

  # Add edge ID values to `graph`
  if (nrow(graph$edges_df) > 0) {
    graph$edges_df$id <- as.integer(1:nrow(graph$edges_df))
  }

  graph
}
