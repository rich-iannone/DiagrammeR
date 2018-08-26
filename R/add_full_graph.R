#' Add a fully connected graph
#'
#' With a graph object of class \code{dgr_graph}, add a fully connected graph
#'   either with or without loops. If the graph object set as directed, the
#'   added graph will have edges to and from each pair of nodes. In the
#'   undirected case, a single edge will link each pair of nodes.
#' @inheritParams node_edge_aes_data
#' @inheritParams render_graph
#' @param n the number of nodes comprising the fully connected graph.
#' @param type an optional string that describes the entity type for the nodes
#'   to be added.
#' @param label either a vector object of length \code{n} that provides optional
#'   labels for the new nodes, or, a boolean value where setting to \code{TRUE}
#'   ascribes node IDs to the label and \code{FALSE} or \code{NULL} yields a
#'   blank label.
#' @param rel an optional string for providing a relationship label to all new
#'   edges created in the connected graph.
#' @param edge_wt_matrix an optional matrix of \code{n} by \code{n} dimensions
#'   containing values to apply as edge weights. If the matrix has row names or
#'   column names and \code{label = TRUE}, those row or column names will be
#'   used as node label values.
#' @param keep_loops an option to simplify the fully connected graph by removing
#'   loops (edges from and to the same node). The default value is \code{FALSE}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph object
#' # and add a directed and fully
#' # connected graph with 3 nodes
#' # and edges to and from all
#' # pairs of nodes; with the option
#' # `keep_loops = TRUE` nodes
#' # will also have edges from
#' # and to themselves
#' graph <-
#'   create_graph() %>%
#'   add_full_graph(
#'     n = 3, keep_loops = TRUE)
#'
#' # Get node information
#' # from this graph
#' graph %>%
#'   get_node_info()
#'
#' # Using `keep_loops = FALSE`
#' # (the default) will remove
#' # the loops
#' create_graph() %>%
#'   add_full_graph(n = 3) %>%
#'   get_node_info()
#'
#' # Values can be set for
#' # the node `label`, node
#' # `type`, and edge `rel`
#' graph <-
#'   create_graph() %>%
#'   add_full_graph(
#'     n = 3,
#'     type = "connected",
#'     label = c("1st", "2nd", "3rd"),
#'     rel = "connected_to")
#'
#' # Show the graph's node
#' # data frame (ndf)
#' graph %>%
#'   get_node_df()
#'
#' # Show the graph's edge
#' # data frame (edf)
#' graph %>%
#'   get_edge_df()
#'
#' # Create a fully-connected and
#' # directed graph with 3 nodes,
#' # and, where a matrix provides
#' # edge weights; first, create the
#' # matrix (with row names to be
#' # used as node labels)
#' set.seed(23)
#'
#' edge_wt_matrix <-
#'   rnorm(100, 5, 2) %>%
#'   sample(9, FALSE) %>%
#'   round(2) %>%
#'   matrix(
#'     nc = 3,
#'     nr = 3,
#'     dimnames = list(c("a", "b", "c")))
#'
#' # Create the fully-connected
#' # graph (without loops however)
#' graph <-
#'   create_graph() %>%
#'   add_full_graph(
#'     n = 3,
#'     type = "weighted",
#'     label = TRUE,
#'     rel = "related_to",
#'     edge_wt_matrix = edge_wt_matrix,
#'     keep_loops = FALSE)
#'
#' # Show the graph's node
#' # data frame (ndf)
#' graph %>%
#'   get_node_df()
#'
#' # Show the graph's edge
#' # data frame (edf)
#' graph %>%
#'   get_edge_df()
#'
#' # An undirected graph can
#' # also use a matrix with
#' # edge weights, but only
#' # the lower triangle of
#' # that matrix will be used
#' create_graph(directed = FALSE) %>%
#'   add_full_graph(
#'     n = 3,
#'     type = "weighted",
#'     label = TRUE,
#'     rel = "related_to",
#'     edge_wt_matrix = edge_wt_matrix,
#'     keep_loops = FALSE) %>%
#'   get_edge_df()
#' @importFrom dplyr select bind_cols as_tibble
#' @export
add_full_graph <- function(graph,
                           n,
                           type = NULL,
                           label = TRUE,
                           rel = NULL,
                           edge_wt_matrix = NULL,
                           keep_loops = FALSE,
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

  # Get the number of nodes in the graph
  nodes_graph_1 <- graph %>% count_nodes()

  # Get the number of edges in the graph
  edges_graph_1 <- graph %>% count_edges()

  # Create initial adjacency matrix
  adj_matrix <- matrix(1, nrow = n, ncol = n)

  # Remove loops by making the diagonal of the
  # adjacency matrix all 0
  if (keep_loops == FALSE) {
    adj_matrix <-
      adj_matrix -
      diag(1, nrow = nrow(adj_matrix), ncol = ncol(adj_matrix))
  }

  if (is_graph_directed(graph)) {

    # Create a new directed graph based on the
    # adjacency matrix `adj_matrix`
    new_graph <-
      from_adj_matrix(adj_matrix, mode = "directed")

    # If a matrix of edge weights provided, apply those
    # to each of the edges in a row-major fashion
    if (!is.null(edge_wt_matrix)) {

      new_graph <-
        set_edge_attrs(
          new_graph,
          edge_attr = "weight",
          values = as.numeric(edge_wt_matrix)[
            which(as.numeric(adj_matrix) == 1)])
    }
  }

  if (is_graph_directed(graph) == FALSE) {

    new_graph <-
      from_adj_matrix(
        adj_matrix,
        mode = "undirected")

    # If a matrix of edge weights provided, apply those
    # from the bottom triangle to each of the edges in a
    # row-major fashion
    if (!is.null(edge_wt_matrix)) {

      new_graph <-
        set_edge_attrs(
          new_graph,
          edge_attr = "weight",
          values = edge_wt_matrix[
            lower.tri(
              edge_wt_matrix,
              diag = ifelse(keep_loops == FALSE,
                            FALSE, TRUE))])
    }
  }

  # Add label values to nodes
  if (length(label) == 1) {
    if (label == TRUE) {
      new_graph$nodes_df[, 3] <- new_graph$nodes_df[, 1]
    }
  }

  if (length(label) == 1) {
    if (label == FALSE) {
      new_graph$nodes_df[, 3] <-
        as.character(new_graph$nodes_df[, 1])
    }
  }

  if (length(label) > 1) {
    if (length(label) == n) {
      new_graph$nodes_df[, 3] <- label
    }
  }

  if (length(label) == 1) {
    if (label == TRUE) {
      if (!is.null(edge_wt_matrix)) {

        if (!is.null(colnames(edge_wt_matrix))) {
          ewm_names <- colnames(edge_wt_matrix)
        }
        if (!is.null(rownames(edge_wt_matrix))) {
          ewm_names <- rownames(edge_wt_matrix)
        }

        if (length(ewm_names) == n) {
          new_graph$nodes_df[, 3] <- ewm_names
        }
      }
    }
  }

  # Add `type` values to all new nodes
  if (!is.null(type) &
      length(type) == 1) {
    new_graph$nodes_df[, 2] <- type
  }

  # Add `rel` values to all new edges
  if (!is.null(rel) &
      length(rel) == 1) {
    new_graph$edges_df[, 4] <- rel
  }

  # Collect node aesthetic attributes
  if (!is.null(node_aes)) {

    node_aes_tbl <- dplyr::as_tibble(node_aes)

    if (nrow(node_aes_tbl) < nrow(new_graph$nodes_df)) {

      node_aes$index__ <- 1:nrow(new_graph$nodes_df)

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

    if (nrow(node_data_tbl) < nrow(new_graph$nodes_df)) {

      node_data$index__ <- 1:nrow(new_graph$nodes_df)

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

  # Collect edge aesthetic attributes
  if (!is.null(edge_aes)) {

    edge_aes_tbl <- dplyr::as_tibble(edge_aes)

    if (nrow(edge_aes_tbl) < nrow(new_graph$edges_df)) {

      edge_aes$index__ <- 1:nrow(new_graph$edges_df)

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

    if (nrow(edge_data_tbl) < nrow(new_graph$edges_df)) {

      edge_data$index__ <- 1:nrow(new_graph$edges_df)

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

  # Add node aesthetics if available
  if (exists("node_aes_tbl")) {

    new_graph$nodes_df <-
      new_graph$nodes_df %>%
      dplyr::bind_cols(node_aes_tbl)
  }

  # Add node data if available
  if (exists("node_data_tbl")) {

    new_graph$nodes_df <-
      new_graph$nodes_df %>%
      dplyr::bind_cols(node_data_tbl)
  }

  # Add edge aesthetics if available
  if (exists("edge_aes_tbl")) {

    new_graph$edges_df <-
      new_graph$edges_df %>%
      dplyr::bind_cols(edge_aes_tbl)
  }

  # Add edge data if available
  if (exists("edge_data_tbl")) {

    new_graph$edges_df <-
      new_graph$edges_df %>%
      dplyr::bind_cols(edge_data_tbl)
  }

  # If the input graph is not empty, combine graphs
  # using the `combine_graphs()` function
  if (!is_graph_empty(graph)) {

    combined_graph <- combine_graphs(graph, new_graph)

    # Update the `last_node` counter
    combined_graph$last_node <- nodes_created + n

    # Get the updated number of nodes in the graph
    nodes_graph_2 <- combined_graph %>% count_nodes()

    # Get the number of nodes added to
    # the graph
    nodes_added <- nodes_graph_2 - nodes_graph_1

    # Get the updated number of edges in the graph
    edges_graph_2 <- combined_graph %>% count_edges()

    # Get the number of edges added to
    # the graph
    edges_added <- edges_graph_2 - edges_graph_1

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = fcn_name,
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(combined_graph$nodes_df),
        edges = nrow(combined_graph$edges_df),
        d_n = nodes_added,
        d_e = edges_added)

    combined_graph$global_attrs <- global_attrs
    combined_graph$graph_log <- graph_log
    combined_graph$graph_info <- graph_info

    # Write graph backup if the option is set
    if (combined_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = combined_graph)
    }

    return(combined_graph)
  } else {

    # Get the updated number of nodes in the graph
    nodes_graph_2 <- new_graph %>% count_nodes()

    # Get the number of nodes added to
    # the graph
    nodes_added <- nodes_graph_2 - nodes_graph_1

    # Get the updated number of edges in the graph
    edges_graph_2 <- new_graph %>% count_edges()

    # Get the number of edges added to
    # the graph
    edges_added <- edges_graph_2 - edges_graph_1

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = nrow(graph_log) + 1,
        function_used = fcn_name,
        time_modified = time_function_start,
        duration = graph_function_duration(time_function_start),
        nodes = nrow(new_graph$nodes_df),
        edges = nrow(new_graph$edges_df),
        d_n = nodes_added,
        d_e = edges_added)

    new_graph$global_attrs <- global_attrs
    new_graph$graph_log <- graph_log
    new_graph$graph_info <- graph_info

    # Perform graph actions, if any are available
    if (nrow(graph$graph_actions) > 0) {
      graph <-
        graph %>%
        trigger_graph_actions()
    }

    # Write graph backup if the option is set
    if (new_graph$graph_info$write_backups) {
      save_graph_as_rds(graph = new_graph)
    }

    return(new_graph)
  }
}
