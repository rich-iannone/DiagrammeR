#' Delete one or more graph actions stored within a graph object
#'
#' Delete one or more graph actions stored within a graph object of class
#'   \code{dgr_graph}).
#' @inheritParams render_graph
#' @param actions either a vector of integer numbers indicating which actions to
#'   delete (based on \code{action_index} values), or, a character vector
#'   corresponding to \code{action_name} values.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5,
#'     m = 8,
#'     set_seed = 23)
#'
#' # Add three graph actions to the
#' # graph
#' graph <-
#'   graph %>%
#'   add_graph_action(
#'     fcn = "set_node_attr_w_fcn",
#'     node_attr_fcn = "get_pagerank",
#'     column_name = "pagerank",
#'     action_name = "get_pagerank") %>%
#'   add_graph_action(
#'     fcn = "rescale_node_attrs",
#'     node_attr_from = "pagerank",
#'     node_attr_to = "width",
#'     action_name = "pagerank_to_width") %>%
#'   add_graph_action(
#'     fcn = "colorize_node_attrs",
#'     node_attr_from = "width",
#'     node_attr_to = "fillcolor",
#'     action_name = "pagerank_fillcolor")
#'
#' # View the graph actions for the graph
#' # object by using the `get_graph_actions()`
#' # function
#' graph %>% get_graph_actions()
#'
#' # Delete the second and third graph
#' # actions using `delete_graph_actions()`
#' graph <-
#'   graph %>%
#'   delete_graph_actions(
#'     actions = c(2, 3))
#'
#' # Verify that these last two graph
#' # actions were deleted by again using
#' # the `get_graph_actions()` function
#' graph %>% get_graph_actions()
#' @importFrom dplyr pull filter mutate row_number
#' @export
delete_graph_actions <- function(graph,
                                 actions) {

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

  # Determine whether there any
  # available graph actions
  if (nrow(graph$graph_actions) == 0) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "There are no graph actions to delete")
  }

  # Create bindings for specific variables
  # action_name <- action_index <- NULL

  if (inherits(actions, "character")) {

    graph_action_names <-
      graph %>%
      get_graph_actions() %>%
      dplyr::pull(action_name)

    if (!any(actions %in% graph_action_names)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "One or more provided `actions` do not exist in the graph")
    }

    # Get a revised data frame with graph actions
    revised_graph_actions <-
      graph %>%
      get_graph_actions() %>%
      dplyr::filter(!(action_name %in% actions)) %>%
      dplyr::mutate(action_index = row_number())
  }

  if (inherits(actions, "numeric")) {

    graph_action_indices <-
      graph %>%
      get_graph_actions() %>%
      dplyr::pull(action_index)

    if (!any(actions %in% graph_action_indices)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "One or more provided `actions` do not exist in the graph")
    }

    # Get a revised data frame with graph actions
    revised_graph_actions <-
      graph %>%
      get_graph_actions() %>%
      dplyr::filter(!(action_index %in% actions)) %>%
      dplyr::mutate(action_index = row_number())
  }

  # Replace `graph$graph_actions` with the
  # revised version
  graph$graph_actions <- revised_graph_actions

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
