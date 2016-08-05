#' Place a vector in graph's cache
#' @description Place any in the cache of a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param to_cache any vector.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     10, 22, set_seed = 1)
#'
#' # Get the closeness values for all nodes from `1`
#' # to `10` and store in the graph's cache
#' graph <-
#'   graph %>%
#'   set_cache(get_closeness(.), "closeness")
#'
#' # Get the graph's cache
#' get_cache(graph)
#' @export set_cache

set_cache <- function(graph,
                      to_cache,
                      col = NULL) {

  if (inherits(to_cache, c("numeric", "character"))) {
    # Store the vector in the graph's cache
    graph$cache <- to_cache
  }

  if (inherits(to_cache, "data.frame")) {

    if (is.null(col)) {
      stop("You must provide a column name from the data frame.")
    }

    if (!is.null(col)) {
      # Extract the vector from the data frame
      # and store in the graph's cache
      graph$cache <- to_cache[, which(colnames(to_cache) == col)]
    }
  }

  # Store the vector in the graph's cache
  return(graph)
}
