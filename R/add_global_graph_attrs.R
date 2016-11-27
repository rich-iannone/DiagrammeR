#' Add one or more global graph attributes
#' @description Add global attributes of a specific
#' type (either \code{graph_attrs}, \code{node_attrs},
#' or \code{edge_attrs} for a graph object of class
#' \code{dgr_graph}).
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param attr the name of the attribute to
#' set for the \code{type} of global attribute
#' specified.
#' @param value the value to be set for the chosen
#' attribute specified in the \code{attr_for_type}
#' argument.
#' @param attr_type the specific type of global graph
#' attribute to set. The type is specified with
#' \code{graph}, \code{node}, or \code{edge}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and set some global attributes
#' graph <-
#'   create_graph() %>%
#'   set_global_graph_attrs(
#'     "overlap", "true", "graph")
#'
#' # Verify that the global attributes have been set
#' get_global_graph_attrs(graph)
#' #>      attr value attr_type
#' #> 1 overlap  true     graph
#'
#' # Add to this set with by using the
#' # `add_global_graph_attrs()` function and then
#' # view the collection of attributes
#' graph <-
#'   graph %>%
#'   add_global_graph_attrs(
#'     "penwidth", 12, "node")
#'
#' get_global_graph_attrs(graph)
#' #>       attr value attr_type
#' #> 1  overlap  true     graph
#' #> 2 penwidth    12      node
#'
#' # When adding an attribute where `attr`
#' # and `attr_type` exists, the value provided
#' # will serve as an update
#' graph %>%
#'   add_global_graph_attrs(
#'     "penwidth", 15, "node") %>%
#'   get_global_graph_attrs()
#' #>       attr value attr_type
#' #> 1  overlap  true     graph
#' #> 2 penwidth    15      node
#' @importFrom dplyr full_join transmute coalesce select
#' @importFrom tibble tibble
#' @export add_global_graph_attrs

add_global_graph_attrs <- function(graph,
                                   attr,
                                   value,
                                   attr_type) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  value.x <- value.y <- NULL

  # Coerce any logical value for `value` to a
  # lowercase character value
  if (length(value) == 1) {
    if (inherits(value, "logical") &
        value %in% c(TRUE, FALSE)) {
      value <- tolower(as.character(value))
    }
  }

  # Create a table for the attributes
  global_attrs_to_add <-
    tibble::tibble(
      attr = as.character(attr),
      value = as.character(value),
      attr_type = as.character(attr_type)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  # Get the global graph attributes already set
  # in the graph object
  global_attrs_available <- graph$global_attrs

  # Join the new attributes to those available
  # on the `attr` and `attr_type` columns
  global_attrs_joined <-
    global_attrs_available %>%
    dplyr::full_join(global_attrs_to_add,
                     by = c("attr",
                            "attr_type")) %>%
    dplyr::transmute(
      attr, attr_type,
      value = dplyr::coalesce(value.y, value.x)) %>%
    dplyr::select(attr, value, attr_type)

  # Replace the graph's global attributes with
  # the revised set
  graph$global_attrs <- global_attrs_joined

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_global_graph_attrs",
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
