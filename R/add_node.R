#' Add a node to an existing graph object
#'
#' With a graph object of class `dgr_graph`, add a new node to the graph.
#'   One can optionally provide node attributes for the created node. There is
#'   also the option to create edges to and from existing nodes in the graph.
#'   Because new edges can also be created through this function, there is the
#'   possibility to set edge attributes for any new graph edges.
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param type an optional character object that acts as a group identifier for
#'   the node to be added.
#' @param label an optional character object that describes the node.
#' @param from an optional vector containing node IDs from which edges will be
#'   directed to the new node.
#' @param to an optional vector containing node IDs to which edges will be
#'   directed from the new node.
#' @return a graph object of class `dgr_graph`.
#' @examples
#' # Create an empty graph and add 2 nodes by using
#' # the `add_node()` function twice
#' graph <-
#'   create_graph() %>%
#'   add_node() %>%
#'   add_node()
#'
#' # Get a count of all nodes
#' # in the graph
#' graph %>% count_nodes()
#'
#' # The nodes added were given
#' # ID values `1` and `2`; obtain
#' # the graph's node IDs
#' graph %>% get_node_ids()
#'
#' # Add a node with a `type`
#' # value defined
#' graph <-
#'   graph %>%
#'   add_node(type = "person")
#'
#' # View the graph's internal
#' # node data frame (ndf)
#' graph %>% get_node_df()
#' @importFrom dplyr select bind_cols as_tibble
#' @export
add_node <- function(graph,
                     type = NULL,
                     label = NULL,
                     from = NULL,
                     to = NULL,
                     node_aes = NULL,
                     edge_aes = NULL,
                     node_data = NULL,
                     edge_data = NULL) {

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

  # Get the node ID for the node to be added
  node <- graph$last_node + 1

  # Get the number of edges in the graph
  edges_in_graph <- nrow(graph$edges_df)

  # Get the number of edges ever created for
  # this graph
  edges_created <- graph$last_edge

  if (is.null(type)) {
    type <- as.character(NA)
  }

  if (is.null(label)) {
    label <- as.character(NA)
  }

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) == 1) {

      node_aes$index__ <- 1

      node_aes_tbl <-
        dplyr::as_tibble(node_aes) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(node_aes_tbl)) {
      node_aes_tbl <-
        node_aes_tbl %>%
        dplyr::select(-id)
    }
  }

  # Collect node data attributes
  if (!is.null(node_data)) {

    node_data_tbl <- dplyr::as_tibble(node_data)

    if (nrow(node_data_tbl) == 1) {

      node_data$index__ <- 1

      node_data_tbl <-
        dplyr::as_tibble(node_data) %>%
        dplyr::select(-index__)
    }

    if ("id" %in% colnames(node_data_tbl)) {
      node_data_tbl <-
        node_data_tbl %>%
        dplyr::select(-id)
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

    new_node[1, 1] <- node

    # Add node aesthetics if available
    if (exists("node_aes_tbl")) {

      new_node <-
        new_node %>%
        dplyr::bind_cols(node_aes_tbl)
    }

    # Add node data if available
    if (exists("node_data_tbl")) {

      new_node <-
        new_node %>%
        dplyr::bind_cols(node_data_tbl)
    }

    combined_nodes <-
      combine_ndfs(graph$nodes_df, new_node)

    edges_in_graph_2 <- nrow(graph$edges_df)

    graph$nodes_df <- combined_nodes
    graph$last_node <- graph$last_node + 1

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = fcn_name,
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df),
        d_n = 1)

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

      emit_error(
        fcn_name = fcn_name,
        reasons = "The nodes from which edges should be applied to the new node are not available")
    }

    if (from_nodes_available) {

      new_node <-
        create_node_df(
          n = 1,
          label = as.character(label),
          type = as.character(type))

      new_node[1, 1] <- node

      # Add node aesthetics if available
      if (exists("node_aes_tbl")) {

        new_node <-
          new_node %>%
          dplyr::bind_cols(node_aes_tbl)
      }

      # Add node data if available
      if (exists("node_data_tbl")) {

        new_node <-
          new_node %>%
          dplyr::bind_cols(node_data_tbl)
      }

      # Combine the new nodes with those in the graph
      combined_nodes <-
        combine_ndfs(graph$nodes_df, new_node)

      # Collect edge aesthetic attributes
      if (!is.null(edge_aes)) {

        edge_aes_tbl <- dplyr::as_tibble(edge_aes)

        if (nrow(edge_aes_tbl) < length(from)) {

          edge_aes$index__ <- 1:length(from)

          edge_aes_tbl <-
            dplyr::as_tibble(edge_aes) %>%
            dplyr::select(-index__)
        }

        if ("id" %in% colnames(edge_aes_tbl)) {
          edge_aes_tbl <-
            edge_aes_tbl %>%
            dplyr::select(-id)
        }
      }

      # Collect edge data attributes
      if (!is.null(edge_data)) {

        edge_data_tbl <- dplyr::as_tibble(edge_data)

        if (nrow(edge_data_tbl) < length(from)) {

          edge_data$index__ <- 1:length(from)

          edge_data_tbl <-
            dplyr::as_tibble(edge_data) %>%
            dplyr::select(-index__)
        }

        if ("id" %in% colnames(edge_data_tbl)) {
          edge_data_tbl <-
            edge_data_tbl %>%
            dplyr::select(-id)
        }
      }

      new_edges <-
        create_edge_df(
          from = from,
          to = rep(node, length(from)))

      new_edges$id <-
        new_edges$id + edges_created

      # Add edge aesthetics if available
      if (exists("edge_aes_tbl")) {

        new_edges <-
          new_edges %>%
          dplyr::bind_cols(edge_aes_tbl)
      }

      # Add edge data if available
      if (exists("edge_data_tbl")) {

        new_edges <-
          new_edges %>%
          dplyr::bind_cols(edge_data_tbl)
      }

      # Use `bind_rows()` to add the
      # new edges
      combined_edges <-
        dplyr::bind_rows(
          graph$edges_df,
          new_edges)

      edges_in_graph_2 <- nrow(combined_edges)
      edges_added <- edges_in_graph_2 - edges_in_graph

      # Create a revised graph
      graph$nodes_df <- combined_nodes
      graph$edges_df <- combined_edges
      graph$last_node <- graph$last_node + 1
      graph$last_edge <- graph$last_edge + edges_added

      # Update the `graph_log` df with an action
      graph$graph_log <-
        add_action_to_log(
          graph_log = graph$graph_log,
          version_id = nrow(graph$graph_log) + 1,
          function_used = fcn_name,
          time_modified = time_function_start,
          duration = graph_function_duration(time_function_start),
          nodes = nrow(graph$nodes_df),
          edges = nrow(graph$edges_df),
          d_n = 1,
          d_e = length(from))

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

      emit_error(
        fcn_name = fcn_name,
        reasons = "The nodes to which edges should be applied from the new node are not available")
    }

    new_node <-
      create_node_df(
        n = 1,
        label = as.character(label),
        type = as.character(type))

    new_node[1, 1] <- node

    # Add node aesthetics if available
    if (exists("node_aes_tbl")) {

      new_node <-
        new_node %>%
        dplyr::bind_cols(node_aes_tbl)
    }

    # Add node data if available
    if (exists("node_data_tbl")) {

      new_node <-
        new_node %>%
        dplyr::bind_cols(node_data_tbl)
    }

    combined_nodes <-
      combine_ndfs(graph$nodes_df, new_node)

    # Collect edge aesthetic attributes
    if (!is.null(edge_aes)) {

      edge_aes_tbl <- dplyr::as_tibble(edge_aes)

      if (nrow(edge_aes_tbl) < length(from)) {

        edge_aes$index__ <- 1:length(from)

        edge_aes_tbl <-
          dplyr::as_tibble(edge_aes) %>%
          dplyr::select(-index__)
      }

      if ("id" %in% colnames(edge_aes_tbl)) {
        edge_aes_tbl <-
          edge_aes_tbl %>%
          dplyr::select(-id)
      }
    }

    # Collect edge data attributes
    if (!is.null(edge_data)) {

      edge_data_tbl <- dplyr::as_tibble(edge_data)

      if (nrow(edge_data_tbl) < length(from)) {

        edge_data$index__ <- 1:length(from)

        edge_data_tbl <-
          dplyr::as_tibble(edge_data) %>%
          dplyr::select(-index__)
      }

      if ("id" %in% colnames(edge_data_tbl)) {
        edge_data_tbl <-
          edge_data_tbl %>%
          dplyr::select(-id)
      }
    }

    new_edges <-
      create_edge_df(
        from = rep(node, length(to)),
        to = to)

    new_edges$id <-
      new_edges$id + edges_created

    # Add edge aesthetics if available
    if (exists("edge_aes_tbl")) {

      new_edges <-
        new_edges %>%
        dplyr::bind_cols(edge_aes_tbl)
    }

    # Add edge data if available
    if (exists("edge_data_tbl")) {

      new_edges <-
        new_edges %>%
        dplyr::bind_cols(edge_data_tbl)
    }

    # Use `bind_rows()` to add the
    # new edges
    combined_edges <-
      dplyr::bind_rows(
        graph$edges_df,
        new_edges)

    edges_in_graph_2 <- nrow(combined_edges)
    edges_added <- edges_in_graph_2 - edges_in_graph

    # Create a revised graph
    graph$nodes_df <- combined_nodes
    graph$edges_df <- combined_edges
    graph$last_node <- graph$last_node + 1
    graph$last_edge <- graph$last_edge + edges_added

    # Update the `graph_log` df with an action
    graph$graph_log <-
      add_action_to_log(
        graph_log = graph$graph_log,
        version_id = nrow(graph$graph_log) + 1,
        function_used = fcn_name,
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df),
        d_n = 1,
        d_e = length(to))

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

      emit_error(
        fcn_name = fcn_name,
        reasons = "The nodes from which edges should be applied to the new node are not available")
    }

    if (to_nodes_available == FALSE) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The nodes to which edges should be applied from the new node are not available")
    }

    if (from_nodes_available & to_nodes_available) {

      new_node <-
        create_node_df(
          n = 1,
          label = as.character(label),
          type = as.character(type))

      new_node[1, 1] <- node

      # Add node aesthetics if available
      if (exists("node_aes_tbl")) {

        new_node <-
          new_node %>%
          dplyr::bind_cols(node_aes_tbl)
      }

      # Add node data if available
      if (exists("node_data_tbl")) {

        new_node <-
          new_node %>%
          dplyr::bind_cols(node_data_tbl)
      }

      # Combine the new nodes with those in the graph
      combined_nodes <-
        combine_ndfs(graph$nodes_df, new_node)

      new_edges <-
        combine_edfs(
          create_edge_df(
            from = rep(node, length(to)),
            to = to),
          create_edge_df(
            from = from,
            to = rep(node, length(from))))

      new_edges$id <-
        new_edges$id + edges_created

      # Collect edge aesthetic attributes
      if (!is.null(edge_aes)) {

        edge_aes_tbl <- dplyr::as_tibble(edge_aes)

        if (nrow(edge_aes_tbl) < (length(from) + length(to))) {

          edge_aes$index__ <- 1:(length(from) + length(to))

          edge_aes_tbl <-
            dplyr::as_tibble(edge_aes) %>%
            dplyr::select(-index__)
        }

        if ("id" %in% colnames(edge_aes_tbl)) {
          edge_aes_tbl <-
            edge_aes_tbl %>%
            dplyr::select(-id)
        }
      }

      # Collect edge data attributes
      if (!is.null(edge_data)) {

        edge_data_tbl <- dplyr::as_tibble(edge_data)

        if (nrow(edge_data_tbl) < (length(from) + length(to))) {

          edge_data$index__ <- 1:(length(from) + length(to))

          edge_data_tbl <-
            dplyr::as_tibble(edge_data) %>%
            dplyr::select(-index__)
        }

        if ("id" %in% colnames(edge_data_tbl)) {
          edge_data_tbl <-
            edge_data_tbl %>%
            dplyr::select(-id)
        }
      }

      # Add edge aesthetics if available
      if (exists("edge_aes_tbl")) {

        new_edges <-
          new_edges %>%
          dplyr::bind_cols(edge_aes_tbl)
      }

      # Add edge data if available
      if (exists("edge_data_tbl")) {

        new_edges <-
          new_edges %>%
          dplyr::bind_cols(edge_data_tbl)
      }

      # Use `bind_rows()` to add the
      # new edges
      combined_edges <-
        dplyr::bind_rows(
          graph$edges_df,
          new_edges)

      edges_in_graph_2 <- nrow(combined_edges)
      edges_added <- edges_in_graph_2 - edges_in_graph

      # Create a revised graph and return that graph
      graph$nodes_df <- combined_nodes
      graph$edges_df <- combined_edges
      graph$last_node <- graph$last_node + 1
      graph$last_edge <- graph$last_edge + edges_added

      # Update the `graph_log` df with an action
      graph$graph_log <-
        add_action_to_log(
          graph_log = graph$graph_log,
          version_id = nrow(graph$graph_log) + 1,
          function_used = fcn_name,
          time_modified = time_function_start,
          duration = graph_function_duration(time_function_start),
          nodes = nrow(graph$nodes_df),
          edges = nrow(graph$edges_df),
          d_n = 1,
          d_e = length(from) + length(to))

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
