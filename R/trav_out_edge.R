#' Traverse from one or more selected nodes onto
#' adjacent, outward edges
#' @description From a graph object of class
#' \code{dgr_graph} move to outgoing edges from a
#' selection of one or more selected nodes, thereby
#' creating a selection of edges. An optional filter
#' by edge attribute can limit the set of edges
#' traversed to.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param conditions an option to use filtering
#' conditions for the traversal.
#' @param copy_attrs_from providing a node attribute
#' name will copy those node attribute values to the
#' traversed edges. If the edge attribute already exists,
#' the values will be merged to the traversed edges;
#' otherwise, a new edge attribute will be created.
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
#' get_node_df(graph)
#' #>   id type label values
#' #> 1  1    a   asd    2.3
#' #> 2  2    a  iekd    4.7
#' #> 3  3    b   idj    9.4
#' #> 4  4    b   edl    8.3
#' #> 5  5    b   ohd    6.3
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
#' # Perform a simple traversal from nodes to
#' # outbound edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(nodes = 1) %>%
#'   trav_out_edge() %>%
#'   get_selection()
#' #> [1] 1 2
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(nodes = 1) %>%
#'   trav_out_edge(
#'     conditions = is.na(rel)) %>%
#'   get_selection()
#' #> [1] 1
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
#' #> [1] 5
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
#' #> [1] 2
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
#' #> [1] 3 4
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
#' #> [1] 4
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
#' #> [1] 3 4
#'
#' # Traverse from node `2` to any outbound
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(nodes = 2) %>%
#'   trav_out_edge(
#'     conditions = grepl("B|C", rel)) %>%
#'   get_selection()
#' #> [1] 3 4
#'
#' # Perform a traversal from all nodes to
#' # their outgoing edges and, while doing
#' # so, copy the `label` node attribute
#' # to any of the nodes' incoming edges
#' graph <-
#'   graph %>%
#'   select_nodes() %>%
#'   trav_out_edge(
#'     copy_attrs_from = "label")
#'
#' # Show the graph's internal edge
#' # data frame after this change
#' graph %>%
#'   get_edge_df()
#' #>   id from to  rel label values
#' #> 1  1    1  2 <NA>   asd   6.00
#' #> 2  1    2  3    A   asd   6.11
#' #> 3  2    3  4    B  iekd   4.72
#' #> 4  2    4  5    C  iekd   6.02
#' #> 5  3    5  5    D   idj   5.05
#' @importFrom dplyr filter select select_ full_join rename everything
#' @importFrom rlang enquo UQ
#' @export trav_out_edge

trav_out_edge <- function(graph,
                          conditions = NULL,
                          copy_attrs_from = NULL) {

  conditions <- rlang::enquo(conditions)

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
  id <- id.y <- from <- to <- rel <- NULL

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
  if (!((rlang::UQ(conditions) %>% paste())[2] == "NULL")) {

    valid_edges <-
      filter(
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

    # If node attribute doesn't exist in the edf,
    # perform a full join
    if (!(copy_attrs_from %in% colnames(edf))) {

      edges <-
        ndf %>%
        dplyr::filter(id %in% starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::right_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = id.y) %>%
        dplyr::select(id, from, to, rel, dplyr::everything())
    }

    # If node attribute exists as a column in the edf
    if (copy_attrs_from %in% colnames(edf)) {

      edges <-
        ndf %>%
        dplyr::filter(id %in% starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::right_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = id) %>%
        dplyr::rename(id = id.y)

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

      # Reorder columns
      edges <-
        edges %>%
        dplyr::select(id, from, to, rel, dplyr::everything())
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
      function_used = "trav_out_edge",
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
