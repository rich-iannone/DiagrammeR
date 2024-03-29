#' Traverse from one or more selected nodes onto adjacent, outward edges
#'
#' @description
#'
#' From a graph object of class `dgr_graph` move to outgoing edges from a
#' selection of one or more selected nodes, thereby creating a selection of
#' edges. An optional filter by edge attribute can limit the set of edges
#' traversed to.
#'
#' This traversal function makes use of an active selection of nodes. After the
#' traversal, depending on the traversal conditions, there will either be a
#' selection of edges or no selection at all.
#'
#' Selections of nodes can be performed using the following node selection
#' (`select_*()`) functions: [select_nodes()], [select_last_nodes_created()],
#' [select_nodes_by_degree()], [select_nodes_by_id()], or
#' [select_nodes_in_neighborhood()].
#'
#' Selections of nodes can also be performed using the following traversal
#' (`trav_*()`) functions: [trav_out()], [trav_in()], [trav_both()],
#' [trav_out_node()], [trav_in_node()], [trav_out_until()], or
#' [trav_in_until()].
#'
#' @inheritParams render_graph
#' @param conditions An option to use filtering conditions for the traversal.
#' @param copy_attrs_from Providing a node attribute name will copy those node
#'   attribute values to the traversed edges. If the edge attribute already
#'   exists, the values will be merged to the traversed edges; otherwise, a new
#'   edge attribute will be created.
#' @param copy_attrs_as If a node attribute name is provided in
#'   `copy_attrs_from`, this option will allow the copied attribute values to be
#'   written under a different edge attribute name. If the attribute name
#'   provided in `copy_attrs_as` does not exist in the graph's edf, the new edge
#'   attribute will be created with the chosen name.
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' # Set a seed
#' suppressWarnings(RNGversion("3.5.0"))
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
#'     rel = c(NA, "A", "B", "C", "D")) %>%
#'   set_node_attrs(
#'     node_attr = values,
#'     values = c(2.3, 4.7, 9.4,
#'                8.3, 6.3))
#'
#' # Create a data frame with node ID values
#' # representing the graph edges (with `from`
#' # and `to` columns), and, a set of numeric values
#' df <-
#'   data.frame(
#'     from = c(1, 1, 2, 2, 3),
#'     to = c(2, 3, 4, 5, 5),
#'     values = round(rnorm(5, 5), 2))
#'
#' # Join the data frame to the graph's internal
#' # edge data frame (edf)
#' graph <-
#'   graph %>%
#'   join_edge_attrs(
#'     df = df)
#'
#' # Show the graph's internal node data frame
#' graph %>% get_node_df()
#'
#' # Show the graph's internal edge data frame
#' graph %>% get_edge_df()
#'
#' # Perform a simple traversal from nodes to
#' # outbound edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(nodes = 1) %>%
#'   trav_out_edge() %>%
#'   get_selection()
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 1) %>%
#'   trav_out_edge(
#'     conditions = is.na(rel)) %>%
#'   get_selection()
#'
#' # Traverse from node `3` to any outbound
#' # edges, filtering to those edges that have
#' # numeric values greater than `5.0` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 3) %>%
#'   trav_out_edge(
#'     conditions = values > 5.0) %>%
#'   get_selection()
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that
#' # have values equal to `A` for the `rel`
#' # edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 1) %>%
#'   trav_out_edge(
#'     conditions = rel == "A") %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any outbound
#' # edges, filtering to those edges that
#' # have values in the set `B` and `C` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_out_edge(
#'     conditions = rel %in% c("B", "C")) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any
#' # outbound edges, and use multiple
#' # conditions for the traversal
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_out_edge(
#'     conditions =
#'       rel %in% c("B", "C") &
#'       values >= 5.0) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any
#' # outbound edges, and use multiple
#' # conditions
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_out_edge(
#'     conditions =
#'       rel %in% c("B", "C") |
#'       values > 6.0) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any outbound
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_out_edge(
#'     conditions = grepl("B|C", rel)) %>%
#'   get_selection()
#'
#' # Perform a traversal from all nodes to
#' # their outgoing edges and, while doing
#' # so, copy the `label` node attribute
#' # to any of the nodes' incoming edges
#' graph <-
#'   graph %>%
#'   select_nodes() %>%
#'   trav_out_edge(
#'     copy_attrs_from = label)
#'
#' # Show the graph's internal edge
#' # data frame after this change
#' graph %>% get_edge_df()
#'
#' @export
trav_out_edge <- function(
    graph,
    conditions = NULL,
    copy_attrs_from = NULL,
    copy_attrs_as = NULL
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  check_graph_valid(graph)

  # Validation: Graph contains nodes
  check_graph_contains_nodes(graph)

  # Validation: Graph contains edges
  check_graph_contains_edges(graph)

  # Validation: Graph object has valid node selection
  check_graph_contains_node_selection(
    graph,
    c("Any traversal requires an active selection.",
      "This type of traversal requires a selection of nodes."))

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

  if (!is.null(copy_attrs_as) && !is.null(copy_attrs_from)) {
    if (copy_attrs_as == copy_attrs_from) {
      copy_attrs_as <- NULL
    }
  }

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_nodes <- graph$node_selection$node

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Get the graph's edge data frame
  edf <- graph$edges_df

  # Find all edges that lead away from the
  # starting nodes and remove edges that
  # are loops
  valid_edges <-
    edf %>%
    dplyr::filter(from %in% starting_nodes) %>%
    dplyr::filter(to != from)

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_edges) == 0) {
    return(graph)
  }

  # If traversal conditions are provided then
  # pass in those conditions and filter the
  # data frame of `valid_edges`
  if (!rlang::quo_is_null(rlang::enquo(conditions))) {

    valid_edges <- dplyr::filter(.data = valid_edges, {{ conditions }})
  }

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_edges) == 0) {
    return(graph)
  }

  # If the option is taken to copy node attribute
  # values to the traversed edges, perform the join
  # operation
  if (!is.null(copy_attrs_from)) {

    ndf_2 <-
      ndf %>%
      dplyr::filter(id %in% starting_nodes) %>%
      dplyr::select("id", !!enquo(copy_attrs_from))

    if (!is.null(copy_attrs_as)) {

      if (copy_attrs_as %in% c("id", "from", "to")) {
        cli::cli_abort(
          "Copied attributes should not overwrite either of the `id`, `from`, or `to` edge attributes.")
      }

      colnames(ndf_2)[2] <- copy_attrs_from <- copy_attrs_as
    }

    # If node attribute doesn't exist in the edf,
    # perform a full join
    if (!(copy_attrs_from %in% colnames(edf))) {

      edges <-
        ndf_2 %>%
        dplyr::right_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = "id.y") %>%
        dplyr::relocate("id", "from", "to", "rel")
    }

    # If the node attribute exists as a column in the edf
    if (copy_attrs_from %in% colnames(edf)) {

      edges <-
        ndf_2 %>%
        dplyr::right_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = "id") %>%
        dplyr::rename(id = "id.y")

      # Get column numbers that end with ".x" or ".y"
      split_var_x_col <-
        grep("\\.x$", colnames(edges))

      split_var_y_col <-
        grep("\\.y$", colnames(edges))

      # Selectively merge in values to the existing
      # edge attribute column
      for (i in seq_len(nrow(edges))) {
        if (!is.na(edges[i, split_var_x_col])) {
          edges[i, split_var_y_col] <- edges[i, split_var_x_col]
        }
      }

      # Rename the ".y" column
      colnames(edges)[split_var_y_col] <- colnames(ndf_2)[2]

      # Drop the ".x" column
      edges <- edges[-split_var_x_col]

      # Reorder columns
      edges <-
        edges %>%
        dplyr::relocate("id", "from", "to", "rel")
    }

    # Update the graph's internal node data frame
    graph$edges_df <- edges
  }

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Add the edge information to the active selection
  # of edges in `graph$edge_selection`
  graph$edge_selection <-
    replace_graph_edge_selection(
      graph = graph,
      edge_id = valid_edges$id,
      from_node = valid_edges$from,
      to_node = valid_edges$to)

  # Replace `graph$node_selection` with an empty df
  graph$node_selection <- create_empty_nsdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1L,
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
