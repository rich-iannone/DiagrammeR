#' Traverse from one or more selected edges onto
#' adjacent, outward nodes
#' @description From a graph object of class
#' \code{dgr_graph} with an active selection of
#' edges move opposite to the edge direction to
#' connected nodes, replacing the current edge selection
#' with those nodes traversed to. An optional filter by
#' node attribute can limit the set of nodes traversed
#' to.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param conditions an option to use filtering
#' conditions for the traversal.
#' @param copy_attrs_from providing an edge attribute
#' name will copy those edge attribute values to the
#' traversed nodes. If the edge attribute already exists,
#' the values will be merged to the traversed nodes;
#' otherwise, a new node attribute will be created.
#' @param agg if an edge attribute is provided
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
#'       to = c(2, 3, 4, 5, 5),
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
#' get_node_df(graph)
#' #>   id type label values
#' #> 1  1    a   asd   8.58
#' #> 2  2    a  iekd   7.22
#' #> 3  3    b   idj   5.95
#' #> 4  4    b   edl   6.71
#' #> 5  5    b   ohd   7.48
#'
#' # Show the graph's internal edge data frame
#' get_edge_df(graph)
#' #>   id from to  rel values
#' #> 1  1    1  2 <NA>   6.00
#' #> 2  2    1  3    A   6.11
#' #> 3  3    2  4    B   4.72
#' #> 4  4    2  5    C   6.02
#' #> 5  5    3  5    D   5.05
#'
#' # Perform a simple traversal from the
#' # edge `1`->`3` to the attached node
#' # in the direction of the edge; here, no
#' # conditions are placed on the nodes
#' # traversed to
#' graph %>%
#'   select_edges(
#'     from = 1,
#'       to = 3) %>%
#'   trav_out_node() %>%
#'   get_selection()
#' #> [1] 1
#'
#' # Traverse from edges `2`->`5` and
#' # `3`->`5` to the attached node along
#' # the direction of the edge; here, the
#' # traversals lead to different nodes
#' graph %>%
#'   select_edges(
#'     from = 2,
#'       to = 5) %>%
#'   select_edges(
#'     from = 3,
#'       to = 5) %>%
#'   trav_out_node() %>%
#'   get_selection()
#' #> [1] 2 3
#'
#' # Traverse from the edge `1`->`3`
#' # to the attached node where the edge
#' # is outgoing, this time filtering
#' # numeric values greater than `7.0` for
#' # the `values` node attribute
#' graph %>%
#'   select_edges(
#'     from = 1,
#'       to = 3) %>%
#'   trav_out_node(
#'     conditions = values > 7.0) %>%
#'   get_selection()
#' #> [1] 1
#'
#' # Traverse from the edge `1`->`3`
#' # to the attached node where the edge
#' # is outgoing, this time filtering
#' # numeric values less than `7.0` for
#' # the `values` node attribute (the
#' # condition is not met so the original
#' # selection of edge `1`->`3` remains)
#' graph %>%
#'   select_edges(
#'     from = 1,
#'       to = 3) %>%
#'   trav_out_node(
#'     conditions = values < 7.0) %>%
#'   get_selection()
#' #> [1] 2
#'
#' # Traverse from the edge `1`->`2`
#' # to node `2`, using multiple conditions
#' graph %>%
#'   select_edges(
#'     from = 1,
#'       to = 2) %>%
#'   trav_out_node(
#'     conditions =
#'       grepl(".*d$", label) |
#'       values < 6.0) %>%
#'   get_selection()
#' #> [1] 1
#'
#' # Create another simple graph to demonstrate
#' # copying of edge attribute values to traversed
#' # nodes
#' graph <-
#'   create_graph() %>%
#'   add_node() %>%
#'   select_nodes() %>%
#'   add_n_nodes_ws(
#'     n = 2,
#'     direction = "from") %>%
#'   clear_selection() %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   set_node_attrs_ws(
#'     node_attr = value,
#'     value = 8) %>%
#'   clear_selection() %>%
#'   select_edges_by_edge_id(edges = 1) %>%
#'   set_edge_attrs_ws(
#'     edge_attr = value,
#'     value = 5) %>%
#'   clear_selection() %>%
#'   select_edges_by_edge_id(edges = 2) %>%
#'   set_edge_attrs_ws(
#'     edge_attr = value,
#'     value = 5) %>%
#'   clear_selection() %>%
#'   select_edges()
#'
#' # Show the graph's internal edge data frame
#' graph %>%
#'   get_edge_df()
#' #>   id from to  rel value
#' #> 1  1    1  2 <NA>     5
#' #> 2  2    1  3 <NA>     5
#'
#' # Show the graph's internal node data frame
#' graph %>%
#'   get_node_df()
#' #>   id type label value
#' #> 1  1 <NA>  <NA>    NA
#' #> 2  2 <NA>  <NA>     8
#' #> 3  3 <NA>  <NA>    NA
#'
#' # Perform a traversal from the edges to
#' # the central node (`1`) while also applying
#' # the edge attribute `value` to the node (in
#' # this case summing the `value` of 5 from
#' # both edges before adding as a node attribute)
#' graph <-
#'   graph %>%
#'   trav_out_node(
#'     copy_attrs_from = value,
#'     agg = "sum")
#'
#' # Show the graph's internal node data frame
#' # after this change
#' graph %>%
#'   get_node_df()
#' #>   id type label value
#' #> 1  1 <NA>  <NA>    10
#' #> 2  2 <NA>  <NA>     8
#' #> 3  3 <NA>  <NA>    NA
#' @importFrom stats as.formula
#' @importFrom dplyr filter distinct left_join right_join semi_join select select_ rename group_by summarize_ everything
#' @importFrom rlang enquo UQ
#' @export trav_out_node

trav_out_node <- function(graph,
                          conditions = NULL,
                          copy_attrs_from = NULL,
                          agg = "sum") {

  conditions <- rlang::enquo(conditions)

  copy_attrs_from <- rlang::enquo(copy_attrs_from)
  copy_attrs_from <- (rlang::UQ(copy_attrs_from) %>% paste())[2]

  if (copy_attrs_from == "NULL") {
    copy_attrs_from <- NULL
  }

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

  # Validation: Graph object has valid edge selection
  if (graph_contains_edge_selection(graph) == FALSE) {
    stop("There is no selection of edges, so, no traversal can occur.")
  }

  # Create bindings for specific variables
  id <- from <- from.y <- type <- label <- NULL

  # Get the selection of edges
  starting_edges <- graph$edge_selection

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # Get the graph's edge data frame
  edf <- graph$edges_df

  # Find all nodes that are connected to the
  # starting edges
  valid_nodes <-
    starting_edges %>%
    dplyr::select(from) %>%
    dplyr::distinct() %>%
    dplyr::left_join(ndf, by = c("from" = "id"))

  # If traversal conditions are provided then
  # pass in those conditions and filter the
  # data frame of `valid_nodes`
  if (!((rlang::UQ(conditions) %>% paste())[2] == "NULL")) {

    valid_nodes <-
      filter(
        .data = valid_nodes,
        rlang::UQ(conditions))
  }

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_nodes) == 0) {
    return(graph)
  }

  # If the option is taken to copy edge attribute
  # values to the traversed nodes, perform the join
  # operation
  if (!is.null(copy_attrs_from)) {

    nodes <-
      starting_edges %>%
      dplyr::semi_join(valid_nodes, by = "from") %>%
      dplyr::left_join(edf, by = c("edge" = "id")) %>%
      dplyr::select_("from.y", copy_attrs_from) %>%
      dplyr::rename(id = from.y) %>%
      dplyr::group_by(id) %>%
      dplyr::summarize_(.dots = setNames(
        list(stats::as.formula(
          paste0("~", agg, "(", copy_attrs_from, ", na.rm = TRUE)"))),
        copy_attrs_from)) %>%
      dplyr::right_join(ndf, by = "id") %>%
      dplyr::select(id, type, label, dplyr::everything()) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # If edge attribute exists as a column in the ndf
    if (copy_attrs_from %in% colnames(ndf)) {

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

      # Reorder columns
      nodes <-
        nodes %>%
        dplyr::select(id, type, label, dplyr::everything())
    }

    # Update the graph's internal node data frame
    graph$nodes_df <- nodes
  }

  # Add the node ID values to the active selection
  # of nodes in `graph$node_selection`
  graph$node_selection <-
    replace_graph_node_selection(
      graph = graph,
      replacement = valid_nodes$from)

  # Replace `graph$edge_selection` with an empty df
  graph$edge_selection <- create_empty_esdf()

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "trav_out_node",
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
