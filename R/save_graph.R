#' Save a graph or graph series to disk
#' @description Save a graph or a graph series object
#' to disk.
#' @param x a graph object of class \code{dgr_graph}
#' or a graph series object of type \code{dgr_graph_1D}.
#' @param file a file name for the graph or
#' graph series. Provide a character string and the
#' \code{.dgr} extension will be applied to it.
#' @export save_graph

save_graph <- function(x,
                       file) {

  if (!inherits(file, "character")) {
    stop("Provide a character string for the file.")
  }

  if (inherits(x, "dgr_graph") |
      inherits(x, "dgr_graph_1D")) {

    # Append the file extension to the file path
    file_name <- file %>% paste0(".dgr")

    # Save the graph or graph series
    saveRDS(x, file = file_name, compress = "xz")

  } else {
    stop("The object provided is not a graph or graph series.")
  }
}
