#' Cache a vector in the graph
#' @description Place any in the cache of a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param to_cache any vector or data frame.
#' @param col if a data frame is provided in
#' \code{to_cache} then a column name from that data
#' frame must provided here.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
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
#' #> [1] 0.07142857 0.07692308 0.06666667 0.07142857
#' #> [5] 0.08333333 0.05882353 0.08333333 0.07692308
#' #> [9] 0.06666667 0.05882353
#'
#' # Get the difference of betweenness and closeness
#' # values for nodes in the graph and store in the
#' # graph's cache
#' graph <-
#'   graph %>%
#'   set_cache(
#'     get_betweenness(.)$betweenness -
#'     get_closeness(.)$closeness)
#'
#' # Get the graph's cache
#' get_cache(graph)
#' #> [1]  6.561905  5.561172  1.838095  3.947619
#' #> [5]  8.073810  1.941176 10.073810  8.780220
#' #> [9]  3.400000  1.107843
#' @export set_cache

set_cache <- function(graph,
                      to_cache,
                      col = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  if (inherits(to_cache, c("numeric", "character"))) {
    # Store the vector in the graph's cache
    graph$cache <- to_cache
  }

  if (inherits(to_cache, "data.frame")) {

    if (is.null(col)) {
      stop("You must provide a column name from the data frame.")
    }

    if (!is.null(col)) {

      if (!(col %in% colnames(to_cache))) {
        stop("The column name provided doesn't exist in the data frame.")
      }

      # Extract the vector from the data frame
      # and store in the graph's cache
      graph$cache <- to_cache[, which(colnames(to_cache) == col)]
    }
  }

  # Store the vector in the graph's cache
  return(graph)
}
