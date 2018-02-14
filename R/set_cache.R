#' Cache a vector in the graph
#' @description Place any vector in the
#' cache of a graph object of class
#' \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param to_cache any vector or data
#' frame. If a data frame is supplied
#' then a single column for the vector
#' to pull must be provided in the
#' \code{col} argument.
#' @param name an optional name for the
#' cached vector.
#' @param col if a data frame is provided
#' in \code{to_cache} then a column name
#' from that data frame must provided here.
#' @return a graph object of class
#' \code{dgr_graph}.
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
#' graph %>%
#'   get_cache(
#'     name = "closeness_vector")
#'
#' # Get the difference of betweenness
#' # and closeness values for nodes in
#' # the graph and store the vector in
#' # the graph's cache
#' graph <-
#'   graph %>%
#'   set_cache(
#'     name = "difference",
#'     to_cache =
#'       get_betweenness(.)$betweenness -
#'         get_closeness(.)$closeness)
#'
#' # Get the graph's cache
#' graph %>%
#'   get_cache(
#'     name = "difference")
#' @export set_cache

set_cache <- function(graph,
                      to_cache,
                      name = NULL,
                      col = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
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

      emit_error(
        fcn_name = fcn_name,
        reasons = "You must provide a column name from the data frame")
    }

    if (!is.null(col)) {

      if (!(col %in% colnames(to_cache))) {

        emit_error(
          fcn_name = fcn_name,
          reasons = "The column name provided doesn't exist in the data frame")
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
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
