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

  check_string(file)

  # Check if the object is a graph or graph series.
  if (!rlang::inherits_any(x, c("dgr_graph", "dgr_graph_1D"))) {
    cli::cli_abort(
      "The object provided is not a graph or graph series."
    )
  }

  # Append the file extension to the file path
  file_name <- file %>% paste0(".dgr")

  # Save the graph or graph series
  saveRDS(x, file = file_name, compress = "xz")
}
