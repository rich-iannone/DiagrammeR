#' Set edge attribute values
#'
#' @description
#'
#' From a graph object of class `dgr_graph`, set edge attribute values for one
#' or more edges.
#'
#' @inheritParams render_graph
#' @param edge_attr The name of the attribute to set.
#' @param values The values to be set for the chosen attribute for the chosen
#'   edges.
#' @param from An optional vector of node IDs from which the edge is outgoing
#'   for filtering list of nodes with outgoing edges in the graph.
#' @param to An optional vector of node IDs from which the edge is incoming for
#'   filtering list of nodes with incoming edges in the graph.
#'
#' @return A graph object of class `dgr_graph`.
#'
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
#' # in the graph
#' graph <-
#'   graph %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "green",
#'     from = c(1, 3),
#'     to = c(4, 1))
#'
#' # Set attribute `color = "blue"`
#' # for all edges in the graph
#' graph <-
#'   graph %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "blue")
#'
#' # Set attribute `color = "pink"`
#' # for all edges in graph outbound
#' # from node with ID value `1`
#' graph <-
#'   graph %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "pink",
#'     from = 1)
#'
#' # Set attribute `color = "black"`
#' # for all edges in graph inbound
#' # to node with ID `1`
#' graph <-
#'   graph %>%
#'   set_edge_attrs(
#'     edge_attr = color,
#'     values = "black",
#'     to = 1)
#'
#' @family edge creation and removal
#'
#' @export
set_edge_attrs <- function(
    graph,
    edge_attr,
    values,
    from = NULL,
    to = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Get the requested `edge_attr`
  edge_attr <-
    rlang::enquo(edge_attr) %>% rlang::get_expr() %>% as.character()

  if (edge_attr %in% c("id", "from", "to")) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "You cannot alter edge ID values or attributes associated with node IDs.")
  }

  if (!is.null(from) && !is.null(to)) {
    if (length(from) != length(to)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The number of values specified in `from` and `to` must be the same.")
    }
  }

  # Extract the graph's edf
  edf <- graph$edges_df

  if (length(values) != 1 &&
      length(values) != nrow(edf)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The length of values provided must either be 1 or that of the number of rows in the edf.")
  }

  # Get the indices for the edge data frame
  # that require modification
  if (is.null(from) && !is.null(to)) {
    indices <-
      which(edf$to %in% to)
  } else if (!is.null(from) && is.null(to)) {
    indices <-
      which(edf$from %in% from)
  } else if (is.null(from) && is.null(to)) {
    indices <- seq_len(nrow(edf))
  } else {
    indices <-
      which((edf$from %in% from) &
              (edf$to %in% to))
  }

  # Apply single value to all target edges
  if (length(values) == 1) {

    if (edge_attr %in% colnames(edf)) {

      edf[indices,
          which(colnames(edf) %in%
                  edge_attr)] <- values
    }

    if (!(edge_attr %in% colnames(edf))) {

      # Add a new column and map the edge attribute
      # value to each of the indices in `edges_df`
      edf <-
        dplyr::mutate(
          edf,
          new_attr__ = ifelse(as.numeric(row.names(edf)) %in%
                                indices, values, NA))

      colnames(edf)[ncol(edf)] <- edge_attr
    }
  }

  if (length(values) == nrow(edf)) {

    if (edge_attr %in% colnames(edf)) {
      edf[, which(colnames(edf) %in%
                    edge_attr)] <- values
    }

    if (!(edge_attr %in% colnames(edf))) {
      edf <-
        cbind(edf,
              rep(NA_character_, nrow(edf)))

      edf[, ncol(edf)] <-
        edf[, ncol(edf)]

      colnames(edf)[ncol(edf)] <- edge_attr

      edf[, ncol(edf)] <- values
    }
  }

  # Update the graph object
  graph$edges_df = edf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
