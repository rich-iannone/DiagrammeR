#' Add a grid of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a grid to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param x the number of nodes in
#' the x direction.
#' @param y the number of nodes in
#' the y direction.
#' @param type an optional string
#' that describes the entity type for
#' the nodes to be added.
#' @param label either a vector object
#' of length \code{x * y} that provides
#' optional labels for the new nodes, or,
#' a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the
#' label and \code{FALSE} yields a blank
#' label.
#' @param rel an optional string for
#' providing a relationship label to
#' all new edges created in the grid.
#' @param ... optional node attributes
#' supplied as vectors.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a new graph and add
#' # a 3 x 3 grid
#' graph <-
#'   create_graph() %>%
#'   add_grid(
#'     x = 3, y = 3,
#'     type = "grid")
#'
#' # Get node information
#' # from this graph
#' node_info(graph)
#' #>   id type label deg indeg outdeg loops
#' #> 1  1 grid     1   2     0      2     0
#' #> 2  2 grid     2   3     1      2     0
#' #> 3  3 grid     3   2     1      1     0
#' #> 4  4 grid     4   3     1      2     0
#' #> 5  5 grid     5   4     2      2     0
#' #> 6  6 grid     6   3     2      1     0
#' #> 7  7 grid     7   2     1      1     0
#' #> 8  8 grid     8   3     2      1     0
#' #> 9  9 grid     9   2     2      0     0
#'
#' # Attributes can be specified
#' # in extra arguments and these
#' # are applied in order; Usually
#' # these attributes are applied
#' # to nodes (e.g., `type` is a
#' # node attribute) but the `rel`
#' # attribute will apply to the
#' # edges
#' graph_w_attrs <-
#'   create_graph() %>%
#'   add_grid(
#'     x = 3, y = 2,
#'     label = c("one", "two",
#'               "three", "four",
#'               "five", "six"),
#'     type = c("a", "a",
#'              "b", "b",
#'              "c", "c"),
#'     value = c(1.2, 8.4,
#'               3.4, 5.2,
#'               6.1, 2.6),
#'     rel = "grid")
#'
#' # Get the graph's node data frame
#' get_node_df(graph_w_attrs)
#' #>   id type label value
#' #> 1  1    a   one   1.2
#' #> 2  2    a   two   8.4
#' #> 3  3    b three   3.4
#' #> 4  4    b  four   5.2
#' #> 5  5    c  five   6.1
#' #> 6  6    c   six   2.6
#'
#' # Get the graph's edge data frame
#' get_edge_df(graph_w_attrs)
#' #>   id from to  rel
#' #> 1  1    1  2 grid
#' #> 2  2    1  4 grid
#' #> 3  3    2  3 grid
#' #> 4  4    2  5 grid
#' #> 5  5    3  6 grid
#' #> 6  6    4  5 grid
#' #> 7  7    5  6 grid
#' @importFrom igraph make_lattice
#' @importFrom tibble as_tibble
#' @importFrom dplyr select bind_cols pull
#' @export add_grid

add_grid <- function(graph,
                     x,
                     y,
                     type = NULL,
                     label = TRUE,
                     rel = NULL,
                     ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Stop if `x` is too small
  if (x < 2)  {
    stop("The value for `x` must be at least 2.")
  }

  # Stop if `y` is too small
  if (y < 2)  {
    stop("The value for `y` must be at least 2.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the number of edges ever created for
  # this graph
  edges_created <- graph$last_edge

  # Get the graph's global attributes
  global_attrs <- graph$global_attrs

  # Get the graph's log
  graph_log <- graph$graph_log

  # Get the graph's info
  graph_info <- graph$graph_info

  # Get the graph's state of being directed
  # or undirected
  graph_directed <- graph$directed

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {

    extras_tbl <- tibble::as_tibble(extras)

    if (nrow(extras_tbl) < (x * y)) {

      n <- x * y

      extras$index__ <- 1:n

      extras_tbl <-
        tibble::as_tibble(extras) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(extras_tbl)) {
      extras_tbl <-
        extras_tbl %>%
        dplyr::select(-id)
    }
  }

  grid <-
    igraph::make_lattice(
      dimvector = c(x, y, 1),
      directed = graph_directed) %>%
    from_igraph()

  # Create a node data frame for the grid graph
  grid_nodes <-
    create_node_df(
      n = x * y,
      type = type,
      label = label)

  # Add extra columns if available
  if (exists("extras_tbl")) {

    grid_nodes <-
      grid_nodes %>%
      dplyr::bind_cols(extras_tbl)
  }

  # Create an edge data frame for the grid graph
  grid_edges <-
    create_edge_df(
      from = grid %>%
        get_edge_df() %>%
        dplyr::pull(from),
      to = grid %>%
        get_edge_df() %>%
        dplyr::pull(to),
      rel = rel)

  # Create the grid graph
  grid_graph <-
    create_graph(
      directed = graph_directed,
      nodes_df = grid_nodes,
      edges_df = grid_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, grid_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(grid_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(grid_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_grid",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df))

    combined_graph$global_attrs <- global_attrs
    combined_graph$graph_log <- graph_log
    combined_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(combined_graph$graph_actions) > 0) {
      combined_graph <-
        combined_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (combined_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = combined_graph)
    }

    return(combined_graph)

  } else {

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_grid",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(grid_graph$nodes_df),
        edges = nrow(grid_graph$edges_df))

    grid_graph$global_attrs <- global_attrs
    grid_graph$graph_log <- graph_log
    grid_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(grid_graph$graph_actions) > 0) {
      grid_graph <-
        grid_graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (grid_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = grid_graph)
    }

    return(grid_graph)
  }
}
