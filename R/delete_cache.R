#' Delete vectors cached in a graph object
#'
#' @description
#'
#' Delete vectors cached in a graph object of class `dgr_graph`.
#'
#' @inheritParams render_graph
#' @param name One or more name of vector objects to delete from the cache. If
#'   none supplied, all cached vectors available in the graph will be deleted.
#'
#' @return A vector.
#'
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
#'
#' @export
delete_cache <- function(
    graph,
    name = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # If there are no cached vectors available,
  # return the graph unchanged with a warning
  if (length(graph$cache) == 0) {

    cli::cli_warn(
        "There are no cached vectors, so, the graph is unchanged.",
        call = current_env()
      )

    return(graph)
  }

  if (length(graph$cache) > 0) {

    if (is.null(name)) {

      graph$cache <- list()

    } else {

      if (name %in% names(graph$cache)) {

        graph$cache[name] <- NULL

      } else {

        cli::cli_warn(c(
          "The supplied `name` (`{name}`) does not match the name of a cached vector.",
          "So, the graph is unchanged."
        ), call = current_env())
        return(graph)
      }
    }
  }

  # Get current function
  fcn_name <- get_calling_fcn()

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
