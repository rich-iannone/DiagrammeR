#' Set edge attribute values
#' @description From a graph object of class
#' \code{dgr_graph}, set edge attribute
#' values for one or more edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr the name of the attribute to set.
#' @param values the values to be set for the chosen
#' attribute for the chosen edges.
#' @param from an optional vector of node IDs from
#' which the edge is outgoing for filtering list of
#' nodes with outgoing edges in the graph.
#' @param to an optional vector of node IDs from which
#' the edge is incoming for filtering list of nodes
#' with incoming edges in the graph.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Set attribute `color = "green"`
#' # for edges `1`->`4` and `3`->`1`
#' # using the graph object
#' graph <-
#'   set_edge_attrs(
#'     x = graph,
#'     edge_attr = color,
#'     values = "green",
#'     from = c(1, 3),
#'     to = c(4, 1))
#'
#' # Set attribute `color = "green"`
#' # for edges `1`->`4` and `3`->`1`
#' # using the edge data frame
#' edges <-
#'   set_edge_attrs(
#'     x = edf,
#'     edge_attr = color,
#'     values = "green",
#'     from = c(1, 3),
#'     to = c(4, 1))
#'
#' # Set attribute `color = "blue"`
#' # for all edges in the graph
#' graph <-
#'   set_edge_attrs(
#'     x = graph,
#'     edge_attr = color,
#'     values = "blue")
#'
#' # Set attribute `color = "pink"`
#' # for all edges in graph outbound
#' # from node with ID value `1`
#' graph <-
#'   set_edge_attrs(
#'     x = graph,
#'     edge_attr = color,
#'     values = "pink",
#'     from = 1)
#'
#' # Set attribute `color = "black"`
#' # for all edges in graph inbound
#' # to node with ID `1`
#' graph <-
#'   set_edge_attrs(
#'     x = graph,
#'     edge_attr = color,
#'     values = "black",
#'     to = 1)
#' @importFrom rlang enquo UQ
#' @export set_edge_attrs

set_edge_attrs <- function(graph,
                           edge_attr,
                           values,
                           from = NULL,
                           to = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  edge_attr <- rlang::enquo(edge_attr)
  edge_attr <- (rlang::UQ(edge_attr) %>% paste())[2]

  if (edge_attr %in% c("id", "from", "to")) {

    stop(
      "You cannot alter edge ID values or attributes associated with node IDs.",
      call. = FALSE)
  }

  if (!is.null(from) & !is.null(to)) {
    if (length(from) != length(to)) {

      stop(
        "The number of values specified in `from` and `to` must be the same.",
        call. = FALSE)
    }
  }

  # Extract the graph's edf
  edges_df <- x$edges_df

  if (length(values) != 1 &
      length(values) != nrow(edges_df)) {

    stop(
      "The length of values provided must either be 1 or that of the number of rows in the edf.",
      call. = FALSE)
  }

  # Get the indices for the edge data frame
  # that require modification
  if (is.null(from) & !is.null(to)) {
    indices <-
      which(edges_df$to %in% to)
  } else if (!is.null(from) & is.null(to)) {
    indices <-
      which(edges_df$from %in% from)
  } else if (is.null(from) & is.null(to)) {
    indices <- 1:nrow(edges_df)
  } else {
    indices <-
      which((edges_df$from %in% from) &
              (edges_df$to %in% to))
  }

  # Apply single value to all target edges
  if (length(values) == 1) {

    if (edge_attr %in% colnames(edges_df)) {

      edges_df[indices,
               which(colnames(edges_df) %in%
                       edge_attr)] <- values
    }

    if (!(edge_attr %in% colnames(edges_df))) {

      # Add a new column and map the edge attribute
      # value to each of the indices in `edges_df`
      edges_df <-
        dplyr::mutate(
          edges_df,
          new_attr__ = ifelse(as.numeric(row.names(edges_df)) %in%
                                indices, values, NA))

      colnames(edges_df)[ncol(edges_df)] <- edge_attr
    }
  }

  if (length(values) == nrow(edges_df)) {

    if (edge_attr %in% colnames(edges_df)) {
      edges_df[, which(colnames(edges_df) %in%
                         edge_attr)] <- values
    }

    if (!(edge_attr %in% colnames(edges_df))) {
      edges_df <-
        cbind(edges_df,
              rep(as.character(NA), nrow(edges_df)))

      edges_df[, ncol(edges_df)] <-
        edges_df[, ncol(edges_df)]

      colnames(edges_df)[ncol(edges_df)] <- edge_attr

      edges_df[, ncol(edges_df)] <- values
    }
  }

  # Update the graph object
  x$edges_df = edges_df

  # Update the `graph_log` df with an action
  x$graph_log <-
    add_action_to_log(
      graph_log = x$graph_log,
      version_id = nrow(x$graph_log) + 1,
      function_used = "set_edge_attrs",
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(x$nodes_df),
      edges = nrow(x$edges_df))

  # Write graph backup if the option is set
  if (x$graph_info$write_backups) {
    save_graph_as_rds(graph = x)
  }

  graph
}
