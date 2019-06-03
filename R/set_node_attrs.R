#' Set node attribute values
#'
#' From a graph object of class `dgr_graph`, set node attribute values for
#'   one or more nodes.
#' @inheritParams render_graph
#' @param node_attr the name of the attribute to set.
#' @param values the values to be set for the chosen attribute for the chosen
#'   nodes.
#' @param nodes an optional vector of node IDs for filtering the list of nodes
#'   present in the graph.
#' @return a graph object of class `dgr_graph`.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Set attribute `color = "green"` for
#' # nodes `1` and `3` using the graph object
#' graph <-
#'   graph %>%
#'   set_node_attrs(
#'     node_attr = color,
#'     values = "green",
#'     nodes = c(1, 3))
#'
#' # View the graph's node data frame
#' graph %>% get_node_df()
#'
#' # Set attribute `color = "blue"` for
#' # all nodes in the graph
#' graph <-
#'   graph %>%
#'   set_node_attrs(
#'     node_attr = color,
#'     values = "blue")
#'
#' # Display the graph's ndf
#' graph %>% get_node_df()
#' @importFrom dplyr mutate
#' @importFrom rlang enquo get_expr UQ
#' @export
set_node_attrs <- function(graph,
                           node_attr,
                           values,
                           nodes = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Get the requested `node_attr`
  node_attr <-
    rlang::enquo(node_attr) %>% rlang::get_expr() %>% as.character()

  # Stop function if `node_attr` is `id`
  # if (node_attr == "id") {
  #   stop(
  #     "You cannot use the value `id` for `node_attr`.",
  #     call. = FALSE)
  # }

  # Extract the graph's ndf
  ndf <- graph$nodes_df

  if (length(values) != 1 &
      length(values) != nrow(ndf)) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The length of values provided must either be 1 or that of the number of rows in the ndf")
  }

  if (length(values) == 1) {
    if (node_attr %in% colnames(ndf)) {
      if (is.null(nodes)) {
        ndf[, which(colnames(ndf) %in%
                           node_attr)] <- values
      } else {
        ndf[which(ndf[, 1] %in% nodes),
                 which(colnames(ndf) %in%
                         node_attr)] <- values
      }
    }

    if (!(node_attr %in% colnames(ndf))) {

      if (is.null(nodes)) {
        # Add a new column and map the node attribute
        # value to every row in `ndf`
        ndf <-
          dplyr::mutate(
            ndf,
            new_attr__ = ifelse(id > 0, values, NA))

        colnames(ndf)[ncol(ndf)] <- node_attr

      } else {
        # Add a new column and map the node attribute
        # value to the correct rows
        ndf <-
          dplyr::mutate(
            ndf,
            new_attr__ = ifelse(id %in% nodes, values, NA))

        colnames(ndf)[ncol(ndf)] <- node_attr
      }
    }
  }

  # If the length of values supplied is the same
  # as the number of rows, insert those values
  # to a new or existing node attribute column
  if (length(values) == nrow(ndf)) {

    if (node_attr %in% colnames(ndf)) {
      ndf[, which(colnames(ndf) %in%
                         node_attr)] <- values
    }

    if (!(node_attr %in% colnames(ndf))) {
      ndf <-
        cbind(ndf,
              rep(as.character(NA), nrow(ndf)))

      ndf[, ncol(ndf)] <-
        ndf[, ncol(ndf)]

      colnames(ndf)[ncol(ndf)] <-
        node_attr

      ndf[, ncol(ndf)] <- values
    }
  }

    # Replace the graph's ndf with the
    # revised version
    graph$nodes_df <- ndf

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
