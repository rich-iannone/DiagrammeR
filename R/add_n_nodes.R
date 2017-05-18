#' Add one or several unconnected nodes to the graph
#' @description Add n new nodes to a graph object of
#' class \code{dgr_graph}. Optionally, set node
#' \code{type} values for the new nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param n the number of new nodes to add to the graph.
#' @param type an optional character vector that
#' provides group identifiers for the nodes to be added.
#' @param label an optional character object that
#' describes the nodes to be added.
#' @param ... optional node attributes supplied as
#' vectors.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 5 nodes; these
#' # nodes will be assigned ID values from `1` to `5`
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(5)
#'
#' # Get the graph's nodes
#' graph %>% get_node_ids()
#' #> [1] 1 2 3 4 5
#' @importFrom dplyr select bind_cols bind_rows
#' @importFrom tibble as_tibble
#' @export add_n_nodes

add_n_nodes <- function(graph,
                        n,
                        type = NULL,
                        label = NULL,
                        ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  if (is.null(type)) {
    type <- as.character(NA)
  }

  if (is.null(label)) {
    label <- as.character(NA)
  }

  # Create bindings for specific variables
  id <- index__ <- NULL

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {

    extras_tbl <- tibble::as_tibble(extras)

    if (nrow(extras_tbl) < n) {

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

  # Create a ndf of the correct length
  new_nodes <-
    create_node_df(
      n = n,
      type = type,
      label = label)

  # Add extra columns if available
  if (exists("extras_tbl")) {

    new_nodes <-
      new_nodes %>%
      dplyr::bind_cols(extras_tbl)
  }

  new_nodes[, 1] <- new_nodes[, 1] + graph$last_node

  graph$nodes_df <-
    dplyr::bind_rows(graph$nodes_df, new_nodes)

  # Update the `last_node` counter
  graph$last_node <- graph$last_node + n

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "add_n_nodes",
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
