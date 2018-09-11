#' Traverse from one or more selected nodes onto adjacent edges
#'
#' From a graph object of class \code{dgr_graph} move to adjacent edges from a
#' selection of one or more selected nodes, thereby creating a selection of
#' edges. An optional filter by edge attribute can limit the set of edges
#' traversed to.
#'
#' This traversal function makes use of an active selection of nodes. After the
#' traversal, depending on the traversal conditions, there will either be a
#' selection of edges or no selection at all.
#'
#' Selections of nodes can be performed using the following node selection
#' (\code{select_*()}) functions:
#' \code{\link{select_nodes}()},
#' \code{\link{select_last_nodes_created}()},
#' \code{\link{select_nodes_by_degree}()},
#' \code{\link{select_nodes_by_id}()}, or
#' \code{\link{select_nodes_in_neighborhood}()}.
#'
#' Selections of nodes can also be performed using the following traversal
#' (\code{trav_*()}) functions:
#' \code{\link{trav_out}()},
#' \code{\link{trav_in}()},
#' \code{\link{trav_both}()},
#' \code{\link{trav_out_node}()},
#' \code{\link{trav_in_node}()},
#' \code{\link{trav_out_until}()}, or
#' \code{\link{trav_in_until}()}.
#' @inheritParams render_graph
#' @param conditions an option to use filtering conditions for the traversal.
#' @param copy_attrs_from providing a node attribute name will copy those node
#'   attribute values to the traversed edges. If the edge attribute already
#'   exists, the values will be merged to the traversed edges; otherwise, a new
#'   edge attribute will be created.
#' @param copy_attrs_as if a node attribute name is provided in
#'   \code{copy_attrs_from}, this option will allow the copied attribute values
#'   to be written under a different edge attribute name. If the attribute name
#'   provided in \code{copy_attrs_as} does not exist in the graph's edf, the new
#'   edge attribute will be created with the chosen name.
#' @param agg if a node attribute is provided to \code{copy_attrs_from}, then an
#'   aggregation function is required since there may be cases where multiple
#'   node attribute values will be passed onto the traversed edge(s). To pass
#'   only a single value, the following aggregation functions can be used:
#'   \code{sum}, \code{min}, \code{max}, \code{mean}, or \code{median}.
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
#'   join_edge_attrs(df = df)
#'
#' # Show the graph's internal edge data frame
#' graph %>% get_edge_df()
#'
#' # Perform a simple traversal from nodes to
#' # adjacent edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(nodes = 3) %>%
#'   trav_both_edge() %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_both_edge(
#'     conditions = is.na(rel)) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that have
#' # numeric values greater than `6.5` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_both_edge(
#'     conditions = values > 6.5) %>%
#'   get_selection()
#'
#' # Traverse from node `5` to any adjacent
#' # edges, filtering to those edges that
#' # have values equal to `C` for the `rel`
#' # edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 5) %>%
#'   trav_both_edge(
#'     conditions = rel == "C") %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that
#' # have values in the set `B` and `C` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_both_edge(
#'     conditions = rel %in% c("B", "C")) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use multiple conditions for the
#' # traversal
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_both_edge(
#'     conditions =
#'       rel %in% c("B", "C") &
#'       values > 4.0) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use multiple conditions with
#' # a single-length vector
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_both_edge(
#'     conditions =
#'       rel %in% c("B", "C") |
#'       values > 4.0) %>%
#'   get_selection()
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_both_edge(
#'     conditions = grepl("B|C", rel)) %>%
#'   get_selection()
#'
#' # Create another simple graph to demonstrate
#' # copying of node attribute values to traversed
#' # edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 4) %>%
#'   select_nodes_by_id(nodes = 2:3) %>%
#'   set_node_attrs_ws(
#'     node_attr = value,
#'     value = 5)
#'
#' # Show the graph's internal edge data frame
#' graph %>%get_edge_df()
#'
#' # Show the graph's internal node data frame
#' graph %>% get_node_df()
#'
#' # Perform a traversal from the nodes to
#' # the adjacent edges while also applying
#' # the node attribute `value` to the edges (in
#' # this case summing the `value` of 5 from
#' # all contributing nodes adding as an edge
#' # attribute)
#' graph <-
#'   graph %>%
#'   trav_both_edge(
#'     copy_attrs_from = value,
#'     agg = "sum")
#'
#' # Show the graph's internal edge data frame
#' # after this change
#' graph %>% get_edge_df()
#' @importFrom stats median
#' @importFrom dplyr filter select select_ left_join right_join rename bind_rows group_by summarize_
#' @importFrom tibble as_tibble
#' @importFrom rlang enquo get_expr UQ
#' @export
trav_both_edge <- function(graph,
                           conditions = NULL,
                           copy_attrs_from = NULL,
                           copy_attrs_as = NULL,
                           agg = "sum") {

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

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no nodes")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph contains no edges")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = c(
        "There is no selection of nodes available.",
        "any traversal requires an active selection",
        "this type of traversal requires a selection of nodes"))
  }

  # Capture provided conditions
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

  # Create bindings for specific variables
  from <- to <- id <- id.y <- rel <- e_id <- NULL

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_nodes <- graph$node_selection$node

  # Get the graph's edge data frame
  edf <- graph$edges_df

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Find all edges that lead away from the
  # starting nodes and remove edges that
  # are loops
  valid_edges <-
    edf %>%
    dplyr::filter(from %in% starting_nodes |
                    to %in% starting_nodes) %>%
    dplyr::filter(to != from)

  # If traversal conditions are provided then
  # pass in those conditions and filter the
  # data frame of `valid_edges`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    valid_edges <-
      dplyr::filter(
        .data = valid_edges,
        rlang::UQ(conditions))
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

    if (!(copy_attrs_from %in% colnames(edf))) {

      from_join <-
        ndf %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::filter(id %in% starting_nodes) %>%
        dplyr::right_join(
          valid_edges %>%
            dplyr::select(-rel) %>%
            dplyr::rename(e_id = id),
          by = c("id" = "from")) %>%
        dplyr::select_("e_id", copy_attrs_from)

      to_join <-
        ndf %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::filter(id %in% starting_nodes) %>%
        dplyr::right_join(
          valid_edges %>%
            dplyr::select(-rel) %>%
            dplyr::rename(e_id = id),
          by = c("id" = "to")) %>%
        dplyr::select_("e_id", copy_attrs_from)

      if (!is.null(copy_attrs_as)) {

        if (copy_attrs_as %in% c("id", "from", "to")) {

          emit_error(
            fcn_name = fcn_name,
            reasons = "Copied attributes should not overwrite either of the `id`, `from`, or `to` edge attributes")
        }

        colnames(from_join)[2] <-
          colnames(to_join)[2] <-
          copy_attrs_from <- copy_attrs_as
      }

      edges <-
        dplyr::bind_rows(
          from_join, to_join) %>%
        dplyr::left_join(edf, by = c("e_id" = "id")) %>%
        dplyr::rename(id = e_id) %>%
        dplyr::group_by(id) %>%
        dplyr::summarize_(.dots = setNames(
          list(stats::as.formula(
            paste0("~", agg, "(", copy_attrs_from, ", na.rm = TRUE)"))),
          copy_attrs_from)) %>%
        dplyr::right_join(edf, by = "id") %>%
        dplyr::select(id, from, to, rel, everything()) %>%
        as.data.frame(stringsAsFractions = FALSE)
    }

    if (copy_attrs_from %in% colnames(edf)) {

      from_join <-
        ndf %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::filter(id %in% starting_nodes)

      if (!is.null(copy_attrs_as)) {

        if (copy_attrs_as %in% c("id", "from", "to")) {

          emit_error(
            fcn_name = fcn_name,
            reasons = "Copied attributes should not overwrite either of the `id`, `from`, or `to` edge attributes")
        }

        colnames(from_join)[2] <- copy_attrs_as
      }

      from_join <-
        from_join %>%
        dplyr::right_join(
          valid_edges %>%
            dplyr::select(-rel) %>%
            dplyr::rename(e_id = id),
          by = c("id" = "from"))

      # Get column numbers that end with ".x" or ".y"
      split_var_x_col <-
        which(grepl("\\.x$", colnames(from_join)))

      split_var_y_col <-
        which(grepl("\\.y$", colnames(from_join)))

      # Selectively merge in values to the existing
      # edge attribute column
      for (i in 1:nrow(from_join)) {
        if (!is.na(from_join[i, split_var_x_col])) {
          from_join[i, split_var_y_col] <- from_join[i, split_var_x_col]
        }
      }

      # Rename the ".y" column
      colnames(from_join)[split_var_y_col] <- copy_attrs_from

      # Drop the ".x" column
      from_join <-
        from_join %>%
        dplyr::select_("e_id", copy_attrs_from)

      to_join <-
        ndf %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::filter(id %in% starting_nodes)

      if (!is.null(copy_attrs_as)) {

        colnames(to_join)[2] <- copy_attrs_as
      }

      to_join <-
        to_join %>%
        dplyr::right_join(
          valid_edges %>%
            dplyr::select(-rel) %>%
            dplyr::rename(e_id = id),
          by = c("id" = "to"))

      if (!is.null(copy_attrs_as)) {

        copy_attrs_from <- copy_attrs_as
      }

      # Get column numbers that end with ".x" or ".y"
      split_var_x_col <-
        which(grepl("\\.x$", colnames(to_join)))

      split_var_y_col <-
        which(grepl("\\.y$", colnames(to_join)))

      # Selectively merge in values to the existing
      # edge attribute column
      for (i in 1:nrow(to_join)) {
        if (!is.na(to_join[i, split_var_x_col])) {
          to_join[i, split_var_y_col] <- to_join[i, split_var_x_col]
        }
      }

      # Rename the ".y" column
      colnames(to_join)[split_var_y_col] <- copy_attrs_from

      # Drop the ".x" column
      to_join <-
        to_join %>%
        dplyr::select_("e_id", copy_attrs_from)

      edges <-
        dplyr::bind_rows(
          from_join, to_join) %>%
        dplyr::left_join(edf, by = c("e_id" = "id"))

      # Get column numbers that end with ".x" or ".y"
      split_var_x_col <-
        which(grepl("\\.x$", colnames(edges)))

      split_var_y_col <-
        which(grepl("\\.y$", colnames(edges)))

      # Selectively merge in values to the existing
      # edge attribute column
      for (i in 1:nrow(edges)) {
        if (!is.na(edges[i, split_var_x_col])) {
          edges[i, split_var_y_col] <- edges[i, split_var_x_col]
        }
      }

      # Rename the ".y" column
      colnames(edges)[split_var_y_col] <- copy_attrs_from

      # Drop the ".x" column
      edges <- edges[-split_var_x_col]

      joined_edges <-
        edges %>%
        dplyr::arrange(e_id) %>%
        dplyr::rename(id = e_id) %>%
        dplyr::group_by(id) %>%
        dplyr::summarize_(.dots = setNames(
          list(stats::as.formula(
            paste0("~", agg, "(", copy_attrs_from, ", na.rm = TRUE)"))), copy_attrs_from)) %>%
        dplyr::ungroup() %>%
        dplyr::right_join(edf, by = "id")

      # Get column numbers that end with ".x" or ".y"
      split_var_x_col <-
        which(grepl("\\.x$", colnames(joined_edges)))

      split_var_y_col <-
        which(grepl("\\.y$", colnames(joined_edges)))

      # Selectively merge in values to the existing
      # edge attribute column
      for (i in 1:nrow(joined_edges)) {
        if (!is.na(joined_edges[i, split_var_x_col])) {
          joined_edges[i, split_var_y_col] <- joined_edges[i, split_var_x_col]
        }
      }

      # Rename the ".y" column
      colnames(joined_edges)[split_var_y_col] <- copy_attrs_from

      # Drop the ".x" column
      joined_edges <- joined_edges[-split_var_x_col]

      edges <-
        joined_edges %>%
        dplyr::select(id, from, to, rel, everything()) %>%
        dplyr::arrange(id) %>%
        as.data.frame(stringsAsFractions = FALSE)
    }

    # Update the graph's internal node data frame
    graph$edges_df <- edges
  }

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
