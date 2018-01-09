#' Trigger the execution of a series of graph actions
#' @description Execute the graph actions stored in
#' the graph through the use of the
#' \code{add_graph_action()} function. These actions
#' will be invoked in order and any errors
#' encountered will trigger a warning message and
#' result in no change to the input graph. Normally,
#' graph actions are automatically triggered at
#' every transformation step but this function allows
#' for the manual triggering of graph actions after
#' setting them, for example.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5,
#'     m = 10,
#'     set_seed = 23)
#'
#' # Add a graph action that sets a node
#' # attr column with a function; this
#' # uses the `get_pagerank()` function
#' # to provide PageRank values in the
#' # `pagerank` column
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "set_node_attr_w_fcn",
#'     node_attr_fcn = "get_pagerank",
#'     column_name = "pagerank",
#'     action_name = "get_pagerank")
#'
#' # Add a second graph action (to be
#' # executed after the first one) that
#' # rescales values in the `pagerank`
#' # column between 0 and 1, and, puts
#' # these values in the `width` column
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "rescale_node_attrs",
#'     node_attr_from = "pagerank",
#'     node_attr_to = "width",
#'     action_name = "pgrnk_to_width")
#'
#' # Add a third and final graph action
#' # (to be executed last) that creates
#' # color values in the `fillcolor` column,
#' # based on the numeric values from the
#' # `width` column
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "colorize_node_attrs",
#'     node_attr_from = "width",
#'     node_attr_to = "fillcolor",
#'     action_name = "pgrnk_fillcolor")
#'
#' # View the graph actions for the graph
#' # object by using the `get_graph_actions()`
#' # function
#' graph %>%
#'   get_graph_actions()
#' #> # A tibble: 3 x 3
#' #>   action_index     action_name
#' #>          <dbl>           <chr>
#' #> 1            1    get_pagerank
#' #> 2            2  pgrnk_to_width
#' #> 3            3 pgrnk_fillcolor
#' #> # ... with 1 more variables:
#' #> #   expression <chr>
#'
#' # Manually trigger to invocation of
#' # the graph actions using the
#' # `trigger_graph_actions()` function
#' graph <-
#'   graph %>%
#'   trigger_graph_actions()
#'
#' # Examine the graph's internal node
#' # data frame (ndf) to verify that
#' # the `pagerank`, `width`, and
#' # `fillcolor` columns are present
#' graph %>%
#'   get_node_df()
#' #>   id type label pagerank width fillcolor
#' #> 1  1 <NA>  <NA>   0.2877 0.935   #ABDDA4
#' #> 2  2 <NA>  <NA>   0.0940 0.000   #D7191C
#' #> 3  3 <NA>  <NA>   0.3012 1.000   #2B83BA
#' #> 4  4 <NA>  <NA>   0.1965 0.495   #FFFFBF
#' #> 5  5 <NA>  <NA>   0.1206 0.128   #FDAE61
#' @importFrom dplyr filter pull
#' @export trigger_graph_actions

trigger_graph_actions <- function(graph) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  action_index <- action_name <- NULL

  if (nrow(graph$graph_actions) == 0) {
    message("There are currently no graph actions.")
  } else {

    # Collect text expressions in a vector
    graph_actions <- graph$graph_actions$expression

    # Copy graph state as the `graph_previous` object
    graph_previous <- graph

    expr_error_at_index <- 0

    for (i in 1:length(graph_actions)) {

      if (class(
        tryCatch(
          eval(
            parse(text = graph$graph_actions$expression[i])),
          error = function(x) x))[1] == "simpleError") {
        expr_error_at_index <- i
        break
      } else {
        graph <-
          eval(parse(text = graph$graph_actions$expression[i]))
      }
    }

    if (expr_error_at_index > 0) {

      # Revert `graph_previous` to be the returned
      # graph (because of an evaluation error)
      graph <- graph_previous

      action_name_at_error <-
        graph$graph_actions %>%
        dplyr::filter(action_index == expr_error_at_index) %>%
        dplyr::pull(action_name)

      if (!is.na(action_name_at_error)) {
        message(
          paste0(
            "The series of graph actions was not applied to the graph because ",
            "of an error at action index ", expr_error_at_index, "."))
      } else {
        message(
          paste0(
            "The series of graph actions was not applied to the graph because ",
            "of an error at action index ", expr_error_at_index, " (`",
            action_name_at_error, "`)."))
      }
    }

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "trigger_graph_actions",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

    # Write graph backup if the option is set
    if (graph$graph_info$write_backups) {
      save_graph_as_rds(graph = graph)
    }
  }

  graph
}
