#' Read a graph or graph series from disk
#' @description Load a graph or a graph series object
#' from disk.
#' @param file the filename for the graph or graph
#' series. Optionally, this may contain a path to the
#' file.
#' @export open_graph

open_graph <- function(file) {

  if (!inherits(file, "character")) {
    stop("Provide a character string for the file.")
  }

  # Read the graph or graph series
  x <- readRDS(file = file)

  if (inherits(x, "dgr_graph") |
      inherits(x, "dgr_graph_1D")) {

    return(x)

  } else {
    stop("The object opened is not a graph or graph series.")
  }
}
