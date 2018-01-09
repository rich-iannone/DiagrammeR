#' Cache a vector in the graph
#' @description Place any in the cache of a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param name an optional name for the cached vector.
#' @param to_cache any vector or data frame.
#' @param col if a data frame is provided in
#' \code{to_cache} then a column name from that data
#' frame must provided here.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 22,
#'     set_seed = 23)
#'
#' # Get the closeness values for
#' # all nodes from `1` to `10` and
#' # store in the graph's cache
#' graph <-
#'   graph %>%
#'   set_cache(
#'     name = "closeness_vector",
#'     to_cache = get_closeness(.),
#'     col = "closeness")
#'
#' # Get the graph's cache
#' get_cache(
#'   graph,
#'   name = "closeness_vector")
#' #> [1] 0.07692308 0.08333333 0.07692308 0.06666667
#' #> [5] 0.05555556 0.06250000 0.06666667 0.05000000
#' #> [9] 0.06666667 0.04761905
#'
#' # Get the difference of betweenness and closeness
#' # values for nodes in the graph and store in the
#' # graph's cache
#' graph <-
#'   graph %>%
#'   set_cache(
#'     name = "difference",
#'     to_cache = get_betweenness(.)$betweenness -
#'                get_closeness(.)$closeness)
#'
#' # Get the graph's cache
#' get_cache(
#'   graph,
#'   name = "difference")
#' #> [1]  9.25641026 28.91666667 19.08974359  2.60000000
#' #> [5]  0.44444444 17.93750000 11.93333333 -0.05000000
#' #> [9] 10.26666667 -0.04761905
#' @export set_cache

set_cache <- function(graph,
                      name = NULL,
                      to_cache,
                      col = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  if (inherits(to_cache, c("numeric", "integer", "character"))) {

    # Cache vector in the graph's `cache` list object
    if (!is.null(name)) {
      graph$cache[[name]] <- to_cache
    } else {
      if (length(graph$cache) == 0) {
        graph$cache[[1]] <- to_cache
        names(graph$cache) <- 1
      } else {
        graph$cache[[(length(graph$cache) + 1)]] <-
          to_cache
      }
    }
  }

  if (inherits(to_cache, "data.frame")) {

    if (is.null(col)) {

      stop(
        "You must provide a column name from the data frame.",
        call. = FALSE)
    }

    if (!is.null(col)) {

      if (!(col %in% colnames(to_cache))) {

        stop(
          "The column name provided doesn't exist in the data frame.",
          call. = FALSE)
      }

      # Extract the vector from the data frame and cache
      # vector in the graph's `cache` list object
      if (!is.null(name)) {
        graph$cache[[name]] <- to_cache[, which(colnames(to_cache) == col)]
      } else {
        if (length(graph$cache) == 0) {
          graph$cache[[1]] <- to_cache[, which(colnames(to_cache) == col)]
          names(graph$cache) <- 1
        } else {
          graph$cache[[(length(graph$cache) + 1)]] <-
            to_cache[, which(colnames(to_cache) == col)]
        }
      }
    }
  }

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_cache",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  # Store the vector in the graph's cache
  graph
}
