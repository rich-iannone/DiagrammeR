#' Traverse from one or more selected nodes onto
#' neighboring nodes
#' @description From a graph object of class
#' \code{dgr_graph} move from one or more nodes
#' present in a selection to other nodes that are
#' connected by edges, replacing the current nodes in
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
#' @param agg if a node attribute is provided
#' to \code{copy_attrs_from}, then an aggregation
#' function is required since there may be cases where
#' multiple edge attribute values will be passed onto
#' the traversed node(s). To pass only a single value,
#' the following aggregation functions can be used:
#' \code{sum}, \code{min}, \code{max}, \code{mean}, or
#' \code{median}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Set a seed
#' set.seed(23)
#'
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(
#'     2, type = "a",
#'     label = c("asd", "iekd")) %>%
#'   add_n_nodes(
#'     3, type = "b",
#'     label = c("idj", "edl", "ohd")) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->5",
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
#'   join_edge_attrs(df_edges) %>%
#'   join_node_attrs(df_nodes)
#'
#' get_node_df(graph)
#' #>   id type label values
#' #> 1  1    a   asd   8.58
#' #> 2  2    a  iekd   7.22
#' #> 3  3    b   idj   5.95
#' #> 4  4    b   edl   6.71
#' #> 5  5    b   ohd   7.48
#'
#' get_edge_df(graph)
#' #>   id from to  rel values
#' #> 1  1    1  2 <NA>   6.00
#' #> 2  2    1  3    A   6.11
#' #> 3  3    2  4    B   4.72
#' #> 4  4    2  5    C   6.02
#' #> 5  5    3  5    D   5.05
#'
#' # Perform a simple traversal from node `3`
#' # to adjacent nodes with no conditions on
#' # the nodes traversed to
#' graph %>%
#'   select_nodes_by_id(3) %>%
#'   trav_both() %>%
#'   get_selection()
#' #> [1] 1 5
#'
#' # Traverse from node `2` to any adjacent
#' # nodes, filtering to those nodes that have
#' # numeric values less than `8.0` for
#' # the `values` node attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both(
#'     conditions = "values < 8.0") %>%
#'   get_selection()
#' #> [1] 4 5
#'
#' # Traverse from node `5` to any adjacent
#' # nodes, filtering to those nodes that
#' # have a `type` attribute of `b`
#' graph %>%
#'   select_nodes_by_id(5) %>%
#'   trav_both(
#'     conditions = "type == 'b'") %>%
#'   get_selection()
#' #> [1] 3
#'
#' # Traverse from node `2` to any adjacent
#' # nodes, and use multiple conditions for the
#' # traversal (using a vector in `conditions`
#' # creates a set of `AND` conditions)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both(
#'     conditions = c(
#'       "type == 'a'",
#'       "values > 8.0")) %>%
#'   get_selection()
#' #> [1] 1
#'
#' # Traverse from node `2` to any adjacent
#' # nodes, and use multiple conditions with
#' # a single-length vector (here, using a
#' # `|` to create a set of `OR` conditions)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both(
#'     conditions = c(
#'       "type == 'a' | values > 8.0")) %>%
#'   get_selection()
#' #> [1] 1
#'
#' # Traverse from node `2` to any adjacent
#' # nodes, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both(
#'     conditions = "grepl('..d', label)") %>%
#'   get_selection()
#' #> [1] 1 5
#' @importFrom dplyr filter_ inner_join right_join rename distinct select select_ union group_by summarize_ everything
#' @importFrom tibble as_tibble
#' @export trav_both

trav_both <- function(graph,
                      conditions = NULL,
                      copy_attrs_from = NULL,
                      agg = "sum") {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no traversal can occur.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no traversal can occur.")
  }

  # Validation: Graph object has valid node selection
  if (graph_contains_node_selection(graph) == FALSE) {
    stop("There is no selection of nodes, so, no traversal can occur.")
  }

  # Create bindings for specific variables
  value <- NULL

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_nodes <- graph$node_selection$node

  # Get the graph's edge data frame
  edf <- graph$edges_df

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Find all nodes that are connected to the
  # starting nodes via outgoing edges
  valid_nodes <-
    graph %>%
    get_nbrs(starting_nodes) %>%
    as.integer()

  valid_nodes <-
    tibble::as_tibble(valid_nodes) %>%
    dplyr::rename(id = value) %>%
    dplyr::inner_join(ndf, by = "id") %>%
    dplyr::distinct()

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_nodes) == 0) {
    return(graph)
  }

  # If traversal conditions are provided then
  # pass in those conditions and filter the
  # data frame of `valid_nodes`
  if (!is.null(conditions)) {
    for (i in 1:length(conditions)) {
      valid_nodes <-
        valid_nodes %>%
        dplyr::filter_(conditions[i])
    }
  }

  # If the option is taken to copy node attribute
  # values to the traversed nodes, perform the join
  # operations
  if (!is.null(copy_attrs_from)) {

    from_join <-
      valid_nodes %>%
      dplyr::select(id) %>%
      dplyr::inner_join(edf %>% select(from, to), by = c("id" = "from")) %>%
      dplyr::inner_join(ndf %>% select_("id", copy_attrs_from), by = c("to" = "id")) %>%
      dplyr::select_("id", copy_attrs_from)

    to_join <-
      valid_nodes %>%
      dplyr::select(id) %>%
      dplyr::inner_join(edf %>% select(from, to), by = c("id" = "to")) %>%
      dplyr::inner_join(ndf %>% select_("id", copy_attrs_from), by = c("from" = "id")) %>%
      dplyr::select_("id", copy_attrs_from)

    nodes <-
      from_join %>%
      dplyr::union_all(to_join) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize_(.dots = setNames(
        list(stats::as.formula(
          paste0("~", agg, "(", copy_attrs_from, ", na.rm = TRUE)"))),
        copy_attrs_from)) %>%
      dplyr::right_join(ndf, by = "id") %>%
      dplyr::select(id, type, label, dplyr::everything()) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Get column numbers that end with ".x" or ".y"
    split_var_x_col <-
      which(grepl("\\.x$", colnames(nodes)))

    split_var_y_col <-
      which(grepl("\\.y$", colnames(nodes)))

    # Selectively merge in values to the existing
    # edge attribute column
    for (i in 1:nrow(nodes)) {
      if (!is.na(nodes[i, split_var_x_col])) {
        nodes[i, split_var_y_col] <- nodes[i, split_var_x_col]
      }
    }

    # Rename the ".y" column
    colnames(nodes)[split_var_y_col] <- copy_attrs_from

    # Drop the ".x" column
    nodes <- nodes[-split_var_x_col]

    # Update the graph's internal node data frame
    graph$nodes_df <- nodes
  }

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_nodes) == 0) {
    return(graph)
  }

  # Add the node ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <-
    replace_graph_node_selection(
      graph = graph,
      replacement = valid_nodes$id)

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "trav_both",
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
