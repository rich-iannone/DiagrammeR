#' Save a graph or graph series to disk
#'
#' @description
#'
#' Save a graph or a graph series object to disk.
#'
#' @param x A graph object of class `dgr_graph` or a graph series object of type
#'   `dgr_graph_1D`.
#' @param file A file name for the graph or graph series. Provide a character
#'   string and the `.dgr` extension will be applied to it.
#'
#' @examples
#' # Create an undirected GNP
#' # graph with 100 nodes using
#' # a probability value of 0.05
#' gnp_graph <-
#'   create_graph(
#'     directed = FALSE) %>%
#'   add_gnp_graph(
#'     n = 100,
#'     p = 0.05)
#'
#' # Save the graph to disk; use
#' # the file name `gnp_graph.dgr`
#' # save_graph(
#' #   x = gnp_graph,
#' #   file = "gnp_graph"
#' # )
#'
#' # To read the graph file from
#' # disk, use `open_graph()`
#' # gnp_graph_2 <-
#' #   open_graph(
#' #     file = "gnp_graph.dgr"
#' # )
#'
#' @family Display and Save
#' @export
save_graph <- function(
    x,
    file
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  if (!inherits(file, "character")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "Provide a character string for the file")
  }

  if (inherits(x, "dgr_graph") |
      inherits(x, "dgr_graph_1D")) {

    # Append the file extension to the file path
    file_name <- file %>% paste0(".dgr")

    # Save the graph or graph series
    saveRDS(x, file = file_name, compress = "xz")

  } else {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The object provided is not a graph or graph series")
  }
}
