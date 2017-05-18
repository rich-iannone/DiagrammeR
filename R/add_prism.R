#' Add a prism of nodes to the graph
#' @description With a graph object of class
#' \code{dgr_graph}, add a node prism to the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of nodes describing the shape
#' of the prism. For example, the triagonal prism has
#' \code{n} equal to 3 and it is composed of 6 nodes
#' and 9 edges. For any n-gonal prism, the graph will
#' be generated with 2\code{n} nodes and 3\code{n}
#' edges.
#' @param type an optional string that describes the
#' entity type for the nodes to be added.
#' @param label either a vector object of length
#' \code{n} that provides optional labels for the new
#' nodes, or, a boolean value where setting to
#' \code{TRUE} ascribes node IDs to the label and
#' \code{FALSE} yields a blank label.
#' @param rel an optional string for providing a
#' relationship label to all new edges created in the
#' node prism.
#' @param ... optional node attributes supplied as
#' vectors.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and add 2 prisms
#' graph <-
#'   create_graph() %>%
#'   add_prism(3, "prism", "a") %>%
#'   add_prism(3, "prism", "b")
#'
#' # Get node information from this graph
#' node_info(graph)
#' #>    id  type label deg indeg outdeg loops
#' #> 1   1 prism     a   3     1      2     0
#' #> 2   2 prism     a   3     1      2     0
#' #> 3   3 prism     a   3     1      2     0
#' #> 4   4 prism     a   3     2      1     0
#' #> 5   5 prism     a   3     2      1     0
#' #> 6   6 prism     a   3     2      1     0
#' #> 7   7 prism     b   3     1      2     0
#' #> 8   8 prism     b   3     1      2     0
#' #> 9   9 prism     b   3     1      2     0
#' #> 10 10 prism     b   3     2      1     0
#' #> 11 11 prism     b   3     2      1     0
#' #> 12 12 prism     b   3     2      1     0
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @export add_prism

add_prism <- function(graph,
                      n,
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

  # Stop if n is too small
  if (n <= 2)  {
    stop("The value for n must be at least 3.")
  }

  # Create bindings for specific variables
  id <- index__ <- NULL

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

  # Get the sequence of nodes required
  nodes <- seq(1, 2 * n)

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {

    extras_tbl <- tibble::as_tibble(extras)

    if (nrow(extras_tbl) < length(nodes)) {

      extras$index__ <- 1:length(nodes)

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

  # Create a node data frame for the prism graph
  prism_nodes <-
    create_node_df(
      n = length(nodes),
      type = type,
      label = label)

  # Add extra columns if available
  if (exists("extras_tbl")) {

    prism_nodes <-
      prism_nodes %>%
      dplyr::bind_cols(extras_tbl)
  }

  # Create an edge data frame for the prism graph
  prism_edges <-
    create_edge_df(
      from = c(nodes[1:(length(nodes)/2)],
               nodes[((length(nodes)/2) + 1):length(nodes)],
               nodes[1:(length(nodes)/2)]),
      to = c(nodes[2:(length(nodes)/2)],
             nodes[1],
             nodes[((length(nodes)/2) + 2):length(nodes)],
             nodes[((length(nodes)/2) + 1)],
             nodes[1:(length(nodes)/2)] + n),
      rel = rel)

  # Create the prism graph
  prism_graph <- create_graph(prism_nodes, prism_edges)

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, prism_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + nrow(prism_nodes)

    # Update the `last_edge` counter
    combined_graph$last_edge <- edges_created + nrow(prism_edges)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = "add_prism",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df))

    combined_graph$global_attrs <- global_attrs
    combined_graph$graph_log <- graph_log
    combined_graph$graph_info <- graph_info

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
        function_used = "add_prism",
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(prism_graph$nodes_df),
        edges = nrow(prism_graph$edges_df))

    prism_graph$global_attrs <- global_attrs
    prism_graph$graph_log <- graph_log
    prism_graph$graph_info <- graph_info

    # Write graph backup if the option is set
    if (prism_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = prism_graph)
    }

    return(prism_graph)
  }
}
