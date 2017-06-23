#' Add a clone of an existing edge to the graph
#' @description Add a new edge to a graph object of
#' class \code{dgr_graph} which is a clones of an edge
#' already in the graph. All edge attributes are
#' preserved.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge an edge ID corresponding to the graph edge
#' to be cloned.
#' @param from the outgoing node from which the edge
#' is connected.
#' @param to the incoming nodes to which each edge
#' is connected.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with a path of
#' # nodes; supply a common `rel`
#' # edge attribute for all edges
#' # in this path
#' graph <-
#'   create_graph() %>%
#'   add_path(
#'     n = 3,
#'     rel = "a") %>%
#'   select_last_edges_created() %>%
#'   set_edge_attrs(
#'     edge_attr = "color",
#'     value = "steelblue") %>%
#'   clear_selection()
#'
#' # Display the graph's internal
#' # edge data frame
#' graph %>%
#'   get_edge_df()
#' #>   id from to rel     color
#' #> 1  1    1  2   a steelblue
#' #> 2  2    2  3   a steelblue
#'
#' # Create a new node (`3`) and then
#' # create an edge between it and
#' # node `1` while reusing the edge
#' # attributes of edge `1` -> `2`
#' # (edge ID `1`)
#' graph <-
#'   graph %>%
#'   add_node() %>%
#'   add_edge_clone(
#'     edge = 1,
#'     from = 3,
#'       to = 1)
#'
#' # Display the graph's internal
#' # edge data frame
#' graph %>%
#'   get_edge_df()
#' #>   id from to rel     color
#' #> 1  1    1  2   a steelblue
#' #> 2  2    2  3   a steelblue
#' #> 3  3    3  1   a steelblue
#' @importFrom dplyr filter select
#' @export add_edge_clone

add_edge_clone <- function(graph,
                           edge,
                           from,
                           to) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, no edges attributes can be set.")
  }

  # Stop function if edge is not a single numerical value
  if (length(edge) > 1 | inherits(edge, "character") | inherits(edge, "logical")) {
    stop("The value for `edge` must be a single, numeric value.")
  }

  # Stop function the edge ID does not correspond
  # to an edge in the graph
  if (!(edge %in% graph$edges_df$id)) {
    stop("The value provided in `edge` does not correspond to an edge in the graph.")
  }

  # Create bindings for specific variables
  id <- version_id <- NULL

  # Get the value for the latest `version_id` for
  # graph (in the `graph_log`)
  current_graph_log_version_id <-
    graph$graph_log$version_id %>%
    max()

  # Extract all of the edge attributes
  # (`rel` and additional edge attrs)
  edge_attr_vals <-
    graph %>%
    get_edge_df() %>%
    dplyr::filter(id == edge) %>%
    dplyr::select(4:ncol(.))

  # Create the requested edge
  graph <-
    graph %>%
    add_edge(
      from = from,
      to = to)

  # Create an edge selection for the
  # new edge in the graph
  graph <-
    graph %>%
    select_last_edges_created()

  # Iteratively set node attribute values for
  # the new nodes in the graph
  for (i in 1:ncol(edge_attr_vals)) {
    graph <-
      graph %>%
      set_edge_attrs_ws(
        edge_attr = colnames(edge_attr_vals)[i],
        value = edge_attr_vals[1, i])
  }

  # Clear the graph's active selection
  graph <-
    graph %>%
    clear_selection()

  # # Update the `last_edge` counter
  # graph$last_edge <- graph$last_edge + 1

  # Remove extra items from the `graph_log`
  graph$graph_log <-
    graph$graph_log %>%
    dplyr::filter(version_id <= current_graph_log_version_id)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_edge_clone",
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
