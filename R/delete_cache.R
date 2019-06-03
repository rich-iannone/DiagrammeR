#' Delete vectors cached in a graph object
#'
#' Delete vectors cached in a graph object of class `dgr_graph`.
#' @inheritParams render_graph
#' @param name one or more name of vector objects to delete from the cache. If
#'   none supplied, all cached vectors available in the graph will be deleted.
#' @return a vector.
#' @examples
#' # Create an empty graph
#' graph <-
#'   create_graph()
#'
#' # Cache 3 different vectors inside
#' # the graph object
#' graph <-
#'   graph %>%
#'   set_cache(
#'     name = "a",
#'     to_cache = 1:4) %>%
#'   set_cache(
#'     name = "b",
#'     to_cache = 5:9) %>%
#'   set_cache(
#'     name = "c",
#'     to_cache = 10:14)
#'
#' # Delete cache `b`
#' graph <-
#'   graph %>%
#'   delete_cache(name = "b")
#'
#' # Delete remaining cached vectors
#' graph <-
#'   graph %>%
#'   delete_cache()
#' @importFrom glue glue
#' @export
delete_cache <- function(graph,
                         name = NULL) {

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

  # If there are no cached vectors available,
  # return the graph unchanged with a warning
  if (length(graph$cache) == 0) {

    warning(
      glue::glue(
        "`delete_cache()`: There are no cached vectors, so, the graph is unchanged."),
      call. = FALSE)

    return(graph)
  }

  if (length(graph$cache) > 0) {

    if (is.null(name)) {

      graph$cache <- list()

    } else {

      if (name %in% (graph$cache %>% names())) {

        graph$cache[name] <- NULL

      } else {

        warning(
          glue::glue(
            "`delete_cache()`: The supplied `name` (`{name}`) does not match a \\
            name of any of the cached vectors, so, the graph is unchanged."),
          call. = FALSE)
        return(graph)
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
