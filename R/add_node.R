#' Add a node to an existing graph object
#' @description With a graph object of class
#' \code{dgr_graph}, add a new node of a specified type
#' to extant nodes within the graph.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param type an optional character object that
#' acts as a group identifier for the node to be added.
#' @param label an optional character object that
#' describes the node.
#' @param from an optional vector containing node IDs
#' from which edges will be directed to the new node.
#' @param to an optional vector containing node IDs to
#' which edges will be directed from the new node.
#' @param ... one or more optional, single value vectors
#' for supplying node attributes to the new node.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 2 nodes by using
#' # the `add_node()` function twice
#' graph <-
#'   create_graph() %>%
#'   add_node() %>%
#'   add_node()
#'
#' # Get a count of nodes in the graph
#' node_count(graph)
#' #> [1] 2
#'
#' # The nodes added were given ID values `1` and `2`
#' get_node_ids(graph)
#' #> [1] 1 2
#'
#' # Add a node with a `type` value defined
#' graph <-
#'   add_node(graph, type = "person")
#'
#' # View the graph's internal node data frame (ndf)
#' get_node_df(graph)
#' #>   id   type label
#' #> 1  1   <NA>  <NA>
#' #> 2  2   <NA>  <NA>
#' #> 3  3 person  <NA>
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @export add_node

add_node <- function(graph,
                     type = NULL,
                     label = NULL,
                     from = NULL,
                     to = NULL,
                     ...) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Get the node ID for the node to be added
  node <- graph$last_node + 1

  if (is.null(type)) {
    type <- as.character(NA)
  }

  if (is.null(label)) {
    label <- as.character(NA)
  }

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {
    if (nrow(tibble::as_tibble(extras)) == 1) {
      extras_tbl <- tibble::as_tibble(extras)
    }
  }

  # Modify graph if neither `to` nor `from`
  # values provided
  if (is.null(from) & is.null(to)) {

    new_node <-
      create_node_df(
        n = 1,
        label = as.character(label),
        type = as.character(type))

    # Add extra columns if available
    if (exists("extras_tbl")) {

      new_node <-
        new_node %>%
        dplyr::bind_cols(extras_tbl)
    }

    new_node[1, 1] <- node

    combined_nodes <-
      combine_ndfs(graph$nodes_df, new_node)

    graph$nodes_df <- combined_nodes
    graph$last_node <- graph$last_node + 1

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "add_node",
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

  # Modify graph if only `from` values provided
  if (!is.null(from) & is.null(to)) {

    from_nodes_available <-
      ifelse(all(from %in% get_node_ids(graph)),
             TRUE, FALSE)

    if (from_nodes_available == FALSE) {
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (from_nodes_available) {

      new_node <-
        create_node_df(
          n = 1,
          label = as.character(label),
          type = as.character(type))

      # Add extra columns if available
      if (exists("extras_tbl")) {

        new_node <-
          new_node %>%
          dplyr::bind_cols(extras_tbl)
      }

      new_node[1, 1] <- node

      # Combine the new nodes with those in the graph
      combined_nodes <-
        combine_ndfs(graph$nodes_df, new_node)

      # Combine the new edges with those in the graph
      combined_edges <-
        combine_edfs(
          graph$edges_df,
          create_edge_df(
            from = from,
            to = rep(node, length(from))))

      # Create a revised graph
      graph$nodes_df <- combined_nodes
      graph$edges_df <- combined_edges
      graph$last_node <- graph$last_node + 1

      # Update the `graph_log` df with an action
      graph$graph_log <-
        add_action_to_log(
          graph_log = graph$graph_log,
          version_id = nrow(graph$graph_log) + 1,
          function_used = "add_node",
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

  # Modify graph if only `to` values provided
  if (is.null(from) & !is.null(to)) {

    to_nodes_available <-
      ifelse(all(to %in% get_node_ids(graph)),
             TRUE, FALSE)

    if (to_nodes_available == FALSE) {
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    new_node <-
      create_node_df(
        n = 1,
        label = as.character(label),
        type = as.character(type))

    # Add extra columns if available
    if (exists("extras_tbl")) {

      new_node <-
        new_node %>%
        dplyr::bind_cols(extras_tbl)
    }

    new_node[1, 1] <- node

    combined_nodes <-
      combine_ndfs(graph$nodes_df, new_node)

    combined_edges <-
      combine_edfs(
        graph$edges_df,
        create_edge_df(
          from = rep(node, length(to)),
          to = to))

    # Create a revised graph
    graph$nodes_df <- combined_nodes
    graph$edges_df <- combined_edges
    graph$last_node <- graph$last_node + 1

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = "add_node",
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

  # Modify graph if both `to` and `from` values provided
  if (!is.null(from) & !is.null(to)) {

    from_nodes_available <-
      ifelse(all(from %in% get_node_ids(graph)),
             TRUE, FALSE)

    to_nodes_available <-
      ifelse(all(to %in% get_node_ids(graph)),
             TRUE, FALSE)

    if (from_nodes_available == FALSE) {
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (to_nodes_available == FALSE) {
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    if (from_nodes_available & to_nodes_available) {

      new_node <-
        create_node_df(
          n = 1,
          label = as.character(label),
          type = as.character(type))

      # Add extra columns if available
      if (exists("extras_tbl")) {

        new_node <-
          new_node %>%
          dplyr::bind_cols(extras_tbl)
      }

      new_node[1, 1] <- node

      # Combine the new nodes with those in the graph
      combined_nodes <-
        combine_ndfs(graph$nodes_df, new_node)

      # Combine the new edges with those in the graph
      combined_edges <-
        combine_edfs(
          graph$edges_df,
          create_edge_df(
            from = from,
            to = rep(node, length(from))),
          create_edge_df(
            from = rep(node, length(to)),
            to = to))

      # Create a revised graph and return that graph
      graph$nodes_df <- combined_nodes
      graph$edges_df <- combined_edges
      graph$last_node <- graph$last_node + 1

      # Update the `graph_log` df with an action
      graph$graph_log <-
        add_action_to_log(
          graph_log = graph$graph_log,
          version_id = nrow(graph$graph_log) + 1,
          function_used = "add_node",
          time_modified = time_function_start,
          duration = graph_function_duration(time_function_start),
          nodes = nrow(graph$nodes_df),
          edges = nrow(graph$edges_df))

      # Perform graph actions, if any are available
      if (nrow(graph$graph_actions) > 0) {
        graph <-
          graph %>%
          trigger_graph_actions()
      }

      # Write graph backup if the option is set
      if (graph$graph_info$write_backups) {
        save_graph_as_rds(graph = graph)
      }

      return(graph)
    }
  }
}
