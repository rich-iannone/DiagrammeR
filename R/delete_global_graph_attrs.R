#' Delete one of the global graph attributes stored within a graph object
#'
#' Delete one of the global attributes stored within a graph object of class
#' `dgr_graph`).
#'
#' @inheritParams render_graph
#' @param attr The name of the attribute to delete for the `type` of global
#'   attribute specified.
#' @param attr_type The specific type of global graph attribute to delete. The
#'   type is specified with `graph`, `node`, or `edge`.
#' @return A graph object of class `dgr_graph`.
#' @examples
#' # Create a new graph and add
#' # some extra global graph attrs
#' graph <-
#'   create_graph() %>%
#'   add_global_graph_attrs(
#'     attr = "overlap",
#'     value = "true",
#'     attr_type = "graph") %>%
#'   add_global_graph_attrs(
#'     attr = "penwidth",
#'     value = 3,
#'     attr_type = "node") %>%
#'   add_global_graph_attrs(
#'     attr = "penwidth",
#'     value = 3,
#'     attr_type = "edge")
#'
#' # Inspect the graph's global
#' # attributes
#' graph %>%
#'   get_global_graph_attr_info()
#'
#' # Delete the `penwidth` attribute
#' # for the graph's nodes using the
#' # `delete_global_graph_attrs()` fcn
#' graph <-
#'   graph %>%
#'   delete_global_graph_attrs(
#'     attr = "penwidth",
#'     attr_type = "node")
#'
#' # View the remaining set of global
#' # attributes for the graph
#' graph %>%
#'   get_global_graph_attr_info()
#'
#' @import glue
#' @import rlang
#' @export
delete_global_graph_attrs <- function(graph,
                                      attr = NULL,
                                      attr_type = NULL) {

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

  # If no `attr` or `attr_type` provided then
  # all global graph attributes will be removed
  if (is.null(attr) & is.null(attr_type)) {

    # Clear the global graph attributes data frame
    # by removing all rows from it
    graph$global_attrs <-
      graph$global_attrs[-(1:(nrow(graph$global_attrs))), ]

    message(
      glue::glue("Deleted all existing global graph attributes."))
  }

  # If an `attr` is provided but not an
  # `attr_type`, then delete all of those
  # `attr`s without regard to their type
  if (is.null(attr_type) & !is.null(attr)) {

    # Capture provided attr
    attr <- rlang::enquo(attr)

    graph$global_attrs <-
      graph$global_attrs %>%
      dplyr::filter(!(attr %in% UQ(attr)))
  }

  if (!is.null(attr_type) & is.null(attr)) {

    # Stop function if `attr_type` is not a valid
    # attribute type
    if (!any(attr_type %in% c("graph", "node", "edge"))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The `attr_type` should be either `graph`, `node`, or `edge`")
    }

    # Capture provided `attr_type`
    attr_type <- rlang::enquo(attr_type)

    graph$global_attrs <-
      graph$global_attrs %>%
      dplyr::filter(!(attr_type %in% UQ(attr_type)))
  }

  if (!is.null(attr_type) & !is.null(attr)) {

    # Stop function if `attr_type` is not a valid
    # attribute type
    if (!any(attr_type %in% c("graph", "node", "edge"))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The `attr_type` should be either `graph`, `node`, or `edge`")
    }

    # Get the global graph attributes already set
    # in the graph object
    global_attrs_available <- graph$global_attrs

    # Create a table with a single row for the
    # attribute to remove
    global_attrs_to_remove <-
      dplyr::tibble(
        attr = as.character(attr),
        value = as.character(NA),
        attr_type = as.character(attr_type)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Use the `anti_join()` to remove global attribute
    # rows from the graph
    global_attrs_joined <-
      global_attrs_available %>%
      dplyr::anti_join(
        global_attrs_to_remove,
        by = c("attr", "attr_type"))

    # Replace the graph's global attributes with
    # the revised set
    graph$global_attrs <- global_attrs_joined
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
