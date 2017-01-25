#' Add an edge between nodes in a graph object
#' @description With a graph object of class
#' \code{dgr_graph}, add an edge to nodes within the
#' graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param from the outgoing node from which the edge
#' is connected.
#' @param to the incoming nodes to which each edge
#' is connected.
#' @param rel an optional string specifying the
#' relationship between the
#' connected nodes.
#' @param use_labels an option to use node \code{label}
#' values in \code{from} and \code{to} for defining
#' node connections. Note that this is only possible
#' if all nodes have distinct \code{label} values set
#' and none exist as an empty string.
#' @param allow_multiple_edges an option to allow or
#' disallow the possibility of creating an edge with an
#' edge definition already extant in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 4 nodes
#' graph <-
#'   create_graph() %>%
#'   add_node(label = "one") %>%
#'   add_node(label = "two") %>%
#'   add_node(label = "three") %>%
#'   add_node(label = "four")
#'
#' # Add an edge between those nodes and attach a
#' # relationship to the edge
#' graph <-
#'  add_edge(
#'    graph,
#'    from = 1,
#'    to = 2,
#'    rel = "A")
#'
#' # Use the `edge_info()` function to verify that
#' # the edge has been created
#' edge_info(graph)
#' #>   id from to rel
#' #> 1  1    1  2   A
#'
#' # Add another node and edge to the graph
#' graph <-
#'   graph %>%
#'   add_edge(3, 2, "A")
#'
#' # Verify that the edge has been created by
#' # getting a count of graph edges
#' edge_count(graph)
#' #> [1] 2
#'
#' # Add edges by specifying node `label` values
#' # and setting `use_labels = TRUE`; note
#' # that all nodes must have unique `label`
#' # values to use this option
#' graph <-
#'   graph %>%
#'   add_edge(
#'     "three", "four", "L",
#'     use_labels = TRUE) %>%
#'   add_edge(
#'     "four", "one", "L",
#'     use_labels = TRUE)
#'
#' # Use the `get_edges()` function to verify
#' # that the edges were added
#' get_edges(graph)
#' #> [1] "1->2" "3->2" "3->4" "4->1"
#' @importFrom dplyr bind_rows
#' @export add_edge

add_edge <- function(graph,
                     from,
                     to,
                     rel = NULL,
                     use_labels = FALSE,
                     allow_multiple_edges = TRUE) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, an edge cannot be added.")
  }

  if (length(from) > 1 | length(to) > 1) {
    stop("Only one edge can be specified.")
  }

  if (is.null(rel)) {
    rel <- as.character(NA)
  }

  # If `use_label` is set to TRUE, treat values in
  # list as labels; need to map to node ID values
  if (use_labels) {
    from_to_node_id <-
      translate_to_node_id(
        graph = graph,
        from = from,
        to = to)

    from <- from_to_node_id$from
    to <- from_to_node_id$to
  }

  # If an edge between nodes is requested and that
  # edge exists, stop function if `allow_multiple_edges`
  # is FALSE
  if (allow_multiple_edges == FALSE) {
    if (all(
      !is.na(get_edges(graph,
                       return_type = "vector")))) {
      if (any(
        get_edges(
          graph, return_type = "list")[[1]] == from &
        get_edges(
          graph, return_type = "list")[[2]] == to)) {
        stop("This edge already exists.")
      }
    }
  }

  # Use `bind_rows()` to add an edge
  if (!is.null(graph$edges_df)) {

    combined_edges <-
      dplyr::bind_rows(
        graph$edges_df,
        data.frame(
          id = as.integer(graph$last_edge + 1),
          from = as.integer(from),
          to = as.integer(to),
          rel = as.character(rel),
          stringsAsFactors = FALSE))

    # Use the `combined_edges` object as a
    # replacement for the graph's internal
    # edge data frame
    graph$edges_df <- combined_edges

    # Modify the `last_edge` vector
    graph$last_edge <- as.integer(graph$last_edge + 1)

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "add_edge",
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
}
