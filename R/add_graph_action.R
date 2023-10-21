#' Add a graph action for execution at every transform
#'
#' @description
#'
#' Add a graph function along with its arguments to be run at every graph
#' transformation step.
#'
#' @inheritParams render_graph
#' @param fcn The name of the function to use.
#' @param ... Arguments and values to pass to the named function in `fcn`, if
#'   necessary.
#' @param action_name An optional name for labeling the action.
#'
#' @return A graph object of class `dgr_graph`.
#'
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
#' # Add a graph action that sets a node
#' # attr column with a function; the
#' # main function `set_node_attr_w_fcn()`
#' # uses the `get_betweenness()` function
#' # to provide betweenness values in the
#' # `btwns` column; this action will
#' # occur whenever there is a function
#' # called on the graph that modifies it
#' # (e.g., `add_n_nodes()`)
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "set_node_attr_w_fcn",
#'     node_attr_fcn = "get_betweenness",
#'     column_name = "btwns",
#'     action_name = "get_btwns")
#'
#' # To ensure that the action is
#' # available in the graph, use the
#' # `get_graph_actions()` function
#' graph %>% get_graph_actions()
#'
#' @export
add_graph_action <- function(
    graph,
    fcn,
    ...,
    action_name = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Collect any function arguments into the
  # `fcn_args` list object
  fcn_args <- list(...)

  # Create a character expression for the
  # function to evaluate at every graph
  # transformation step
  if (length(fcn_args) == 0) {
    char_expr <-
      paste0(
        fcn,
        "(graph = graph)")

  } else {

    arg_names <- vector(mode = "character")
    arg_values <- vector(mode = "character")

    for (i in seq_along(fcn_args)) {

      arg_names <- c(arg_names, names(fcn_args)[i])

      arg_value_class <- class(unname(fcn_args)[[i]])

      if (arg_value_class == "character") {

        fcn_arg <- unname(fcn_args)[[i]]

        fcn_arg <- paste0("'", fcn_arg, "'")

        arg_values <-
          c(arg_values, fcn_arg)

      } else {
        arg_values <-
          c(arg_values,
            unname(fcn_args)[[i]])
      }
    }

    # Create a character-based expression
    char_expr <-
      paste0(
        fcn,
        "(graph = graph, ",
        paste(arg_names, "=", arg_values, collapse = ", "),
        ")")
  }

  # Create a data frame row with the new graph action
  new_graph_action <-
    data.frame(
      action_index = ifelse(nrow(graph$graph_actions) == 0, 1,
                            max(graph$graph_actions$action_index) + 1),
      action_name = ifelse(!is.null(action_name), action_name,
                           NA_character_),
      expression = char_expr,
      stringsAsFactors = FALSE)

  # Append `new_graph_action` to `graph$graph_actions`
  graph$graph_actions <-
    dplyr::bind_rows(graph$graph_actions, new_graph_action)

  # Get the name of the function
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
