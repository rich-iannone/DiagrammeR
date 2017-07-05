#' Delete one of the global graph attributes stored
#' within a graph object
#' @description Delete one of the global attributes
#' stored within a graph object of class
#' \code{dgr_graph}).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param attr the name of the attribute to
#' delete for the \code{type} of global attribute
#' specified.
#' @param attr_type the specific type of global graph
#' attribute to delete. The type is specified with
#' \code{graph}, \code{node}, or \code{edge}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add some global attributes
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
#' # View the graph's global attributes
#' get_global_graph_attrs(graph)
#' #>       attr value attr_type
#' #> 1  overlap  true     graph
#' #> 2 penwidth     3      node
#' #> 3 penwidth     3      edge
#'
#' # Delete the `penwidth` attribute for the graph's
#' # nodes using `delete_global_graph_attrs()`
#' graph <-
#'   graph %>%
#'   delete_global_graph_attrs(
#'     attr = "penwidth",
#'     attr_type = "node")
#'
#' # View the remaining set of global
#' # attributes for the graph
#' get_global_graph_attrs(graph)
#' #>       attr value attr_type
#' #> 1 penwidth     3      edge
#' #> 2  overlap  true     graph
#' @importFrom dplyr anti_join
#' @importFrom tibble tibble
#' @export delete_global_graph_attrs

delete_global_graph_attrs <- function(graph,
                                      attr,
                                      attr_type) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop function if `attr_type` is not a valid
  # attribute type
  if (!any(attr_type %in% c("graph", "node", "edge"))) {
    stop("The `attr_type` should be either `graph`, `node`, or `edge`.")
  }

  # Get the global graph attributes already set
  # in the graph object
  global_attrs_available <- graph$global_attrs

  # Create a table with a single row for the
  # attribute to remove
  global_attrs_to_remove <-
    tibble::tibble(
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

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "delete_global_graph_attrs",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  return(graph)
}
