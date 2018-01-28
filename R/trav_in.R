#' Traverse from one or more selected nodes onto
#' adjacent, inward nodes
#' @description From a graph object of class
#' \code{dgr_graph} move along inward edges from one
#' or more nodes present in a selection to other
#' connected nodes, replacing the current nodes in
#' the selection with those nodes traversed to. An
#' optional filter by node attribute can limit the set
#' of nodes traversed to.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param conditions an option to use filtering
#' conditions for the traversal.
#' @param copy_attrs_from providing a node attribute
#' name will copy those node attribute values to the
#' traversed nodes. Any values extant on the nodes
#' traversed to will be replaced.
#' @param copy_attrs_as if a node attribute name
#' is provided in \code{copy_attrs_from}, this option
#' will allow the copied attribute values to be
#' written under a different attribute name. If the
#' attribute name provided in \code{copy_attrs_as}
#' does not exist in the graph's ndf, the new
#' node attribute will be created with the chosen
#' name.
#' @param agg if a node attribute is provided
#' to \code{copy_attrs_from}, then an aggregation
#' function is required since there may be cases where
#' multiple edge attribute values will be passed onto
#' the traversed node(s). To pass only a single value,
#' the following aggregation functions can be used:
#' \code{sum}, \code{min}, \code{max}, \code{mean}, or
#' \code{median}.
#' @param add_to_selection an option to
#' either add the traversed to nodes
#' to the active selection of nodes
#' (\code{TRUE}) or switch the active
#' selection entirely to those traversed
#' to nodes (\code{FALSE}, the default case).
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Set a seed
#' set.seed(23)
#'
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(
#'     n = 2,
#'     type = "a",
#'     label = c("asd", "iekd")) %>%
#'   add_n_nodes(
#'     n = 3,
#'     type = "b",
#'     label = c("idj", "edl", "ohd")) %>%
#'   add_edges_w_string(
#'     edges = "1->2 1->3 2->4 2->5 3->5",
#'     rel = c(NA, "A", "B", "C", "D"))
#'
#' # Create a data frame with node ID values
#' # representing the graph edges (with `from`
#' # and `to` columns), and, a set of numeric values
#' df_edges <-
#'   data.frame(
#'     from = c(1, 1, 2, 2, 3),
#'     to = c(2, 3, 4, 5, 5),
#'     values = round(rnorm(5, 5), 2))
#'
#' # Create a data frame with node ID values
#' # representing the graph nodes (with the `id`
#' # columns), and, a set of numeric values
#' df_nodes <-
#'   data.frame(
#'     id = 1:5,
#'     values = round(rnorm(5, 7), 2))
#'
#' # Join the data frame to the graph's internal
#' # edge data frame (edf)
#' graph <-
#'   graph %>%
#'   join_edge_attrs(df = df_edges) %>%
#'   join_node_attrs(df = df_nodes)
#'
#' # Show the graph's internal node data frame
#' graph %>%
#'   get_node_df()
#'
#' # Show the graph's internal edge data frame
#' graph %>%
#'   get_edge_df()
#'
#' # Perform a simple traversal from node `4` to
#' # inward adjacent edges with no conditions
#' # on the nodes traversed to
#' graph %>%
#'   select_nodes_by_id(nodes = 4) %>%
#'   trav_in() %>%
#'   get_selection()
#'
#' # Traverse from node `5` to inbound-facing
#' # nodes, filtering to those nodes that have
#' # numeric values greater than `5.0` for
#' # the `values` node attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 4) %>%
#'   trav_in(
#'     conditions = values > 5.0) %>%
#'   get_selection()
#'
#' # Traverse from node `5` to any inbound
#' # nodes, filtering to those nodes that
#' # have a `type` attribute of `b`
#' graph %>%
#'   select_nodes_by_id(nodes = 5) %>%
#'   trav_in(
#'     conditions = type == "b") %>%
#'   get_selection()
#'
#' # Traverse from node `5` to any inbound
#' # nodes, filtering to those nodes that
#' # have a degree of `2`
#' graph %>%
#'   {
#'   node_degrees <-
#'     get_node_info(.) %>%
#'     dplyr::select(id, deg)
#'   join_node_attrs(., node_degrees)
#'   } %>%
#'   select_nodes_by_id(nodes = 5) %>%
#'   trav_in(
#'     conditions = deg == 2) %>%
#'   get_selection()
#'
#' # Traverse from node `5` to any inbound
#' # nodes, and use multiple conditions for the
#' # traversal
#' graph %>%
#'   select_nodes_by_id(nodes = 5) %>%
#'   trav_in(
#'     conditions =
#'       type == "a" &
#'       values > 6.0) %>%
#'   get_selection()
#'
#' # Traverse from node `5` to any inbound
#' # nodes, and use multiple conditions with
#' # a single-length vector
#' graph %>%
#'   select_nodes_by_id(nodes = 5) %>%
#'   trav_in(
#'     conditions =
#'       type == "b" | values > 6.0) %>%
#'   get_selection()
#'
#' # Traverse from node `5` to any inbound
#' # nodes, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_in(
#'     conditions = grepl("^i.*", label)) %>%
#'   get_selection()
#'
#' # Create another simple graph to demonstrate
#' # copying of node attribute values to traversed
#' # nodes
#' graph <-
#'   create_graph() %>%
#'   add_node() %>%
#'   select_nodes() %>%
#'   add_n_nodes_ws(
#'     n = 2,
#'     direction = "from") %>%
#'   clear_selection() %>%
#'   select_nodes_by_id(nodes = 2:3) %>%
#'   set_node_attrs_ws(
#'     node_attr = value,
#'     value = 5)
#'
#' # Show the graph's internal node data frame
#' graph %>%
#'   get_node_df()
#'
#' # Show the graph's internal edge data frame
#' graph %>%
#'   get_edge_df()
#'
#' # Perform a traversal from the outer nodes
#' # (`2` and `3`) to the central node (`1`) while
#' # also applying the node attribute `value` to
#' # node `1` (summing the `value` of 5 from
#' # both nodes before applying the value to the
#' # target node)
#' graph <-
#'   graph %>%
#'   trav_in(
#'     copy_attrs_from = value,
#'     agg = "sum")
#'
#' # Show the graph's internal node data frame
#' # after this change
#' graph %>%
#'   get_node_df()
#' @importFrom dplyr filter inner_join right_join rename distinct as_tibble
#' @importFrom dplyr select select_ group_by ungroup summarize_ everything
#' @importFrom rlang enquo UQ get_expr
#' @export trav_in

trav_in <- function(graph,
                    conditions = NULL,
                    copy_attrs_from = NULL,
                    copy_attrs_as = NULL,
                    agg = "sum",
                    add_to_selection = FALSE) {

  conditions <- rlang::enquo(conditions)

  # Get the requested `copy_attrs_from`
  copy_attrs_from <-
    rlang::enquo(copy_attrs_from) %>% rlang::get_expr() %>% as.character()

  # Get the requested `copy_attrs_as`
  copy_attrs_as <-
    rlang::enquo(copy_attrs_as) %>% rlang::get_expr() %>% as.character()

  if (length(copy_attrs_from) == 0) {
    copy_attrs_from <- NULL
  }

  if (length(copy_attrs_as) == 0) {
    copy_attrs_as <- NULL
  }

  if (!is.null(copy_attrs_as) & !is.null(copy_attrs_from)) {
    if (copy_attrs_as == copy_attrs_from) {
      copy_attrs_as <- NULL
    }
  }

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, no traversal can occur.",
      call. = FALSE)
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    stop(
      "The graph contains no edges, so, no traversal can occur.",
      call. = FALSE)
  }

  # Validation: Graph object has a valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    stop(
      "There is no selection of nodes, so, no traversal can occur.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  id <- from <- to <- type <- label <- NULL

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_nodes <- graph$node_selection$node

  # Get the graph's edge data frame
  edf <- graph$edges_df

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Find all nodes that are connected to the
  # starting nodes via incoming edges
  valid_nodes <-
    edf %>%
    dplyr::filter(to != from) %>%
    dplyr::filter(to %in% starting_nodes) %>%
    dplyr::select(from) %>%
    dplyr::distinct()

  valid_nodes <-
    dplyr::as_tibble(valid_nodes) %>%
    dplyr::rename(id = from) %>%
    dplyr::inner_join(ndf, by = "id")

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_nodes) == 0) {
    return(graph)
  }

  # If traversal conditions are provided then
  # pass in those conditions and filter the
  # data frame of `valid_nodes`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    valid_nodes <-
      dplyr::filter(
        .data = valid_nodes,
        rlang::UQ(conditions))
  }

  # If the option is taken to copy node attribute
  # values to the traversed nodes, perform the join
  # operations
  if (!is.null(copy_attrs_from)) {

    nodes <-
      valid_nodes %>%
      dplyr::select(id) %>%
      dplyr::inner_join(edf %>% dplyr::select(from, to), by = c("id" = "from")) %>%
      dplyr::inner_join(ndf %>% dplyr::select_("id", copy_attrs_from), by = c("to" = "id")) %>%
      dplyr::select_("id", copy_attrs_from)

    # If the values to be copied are numeric,
    # perform aggregation on the values
    if (nodes[, 2] %>% unlist() %>% is.numeric()) {
      nodes <-
        nodes %>%
        dplyr::group_by(id) %>%
        dplyr::summarize_(.dots = setNames(
          list(stats::as.formula(
            paste0("~", agg, "(", copy_attrs_from, ", na.rm = TRUE)"))),
          copy_attrs_from)) %>%
        dplyr::ungroup()
    }

    nodes <-
      nodes %>%
      dplyr::right_join(ndf, by = "id") %>%
      dplyr::select(id, type, label, dplyr::everything()) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Get column numbers that end with ".x" or ".y"
    split_var_x_col <-
      which(grepl("\\.x$", colnames(nodes)))

    split_var_y_col <-
      which(grepl("\\.y$", colnames(nodes)))

    if (is.null(copy_attrs_as)) {

      # Selectively merge in values to the existing
      # edge attribute column
      for (i in 1:nrow(nodes)) {
        if (!is.na(nodes[i, split_var_x_col])) {
          nodes[i, split_var_y_col] <- nodes[i, split_var_x_col]
        }
      }
    }

    if (!is.null(copy_attrs_as)) {

      # Reorder the columns generated
      nodes <-
        nodes[, c(c(1:(ncol(nodes) - 2)), split_var_y_col, split_var_x_col)]
    }

    # Rename the ".y" column
    colnames(nodes)[split_var_y_col] <- copy_attrs_from

    if (is.null(copy_attrs_as)) {

      # Drop the ".x" column
      nodes <- nodes[-split_var_x_col]

    } else {

      # Rename the two columns
      colnames(nodes)[split_var_x_col] <- copy_attrs_from
      colnames(nodes)[split_var_y_col] <- copy_attrs_as
    }

    # Update the graph's internal node data frame
    graph$nodes_df <- nodes
  }

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_nodes) == 0) {
    return(graph)
  }

  # Obtain vector with node ID selection of nodes
  # already present
  nodes_prev_selection <- graph$node_selection$node

  if (add_to_selection) {

    # If TRUE supplied to `add_to_selection` add
    # the nodes to which we traversed to the
    # previous selection
    nodes_combined <- union(nodes_prev_selection, valid_nodes$id)

    graph$node_selection <-
      replace_graph_node_selection(
        graph = graph,
        replacement = nodes_combined)

  } else {

    # Add the node ID values to the active selection
    # of nodes in `graph$node_selection`
    graph$node_selection <-
      replace_graph_node_selection(
        graph = graph,
        replacement = valid_nodes$id)
  }

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "trav_in",
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
