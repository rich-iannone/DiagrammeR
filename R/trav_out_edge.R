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
#' df <-
#'   data.frame(
#'     from = c(1, 1, 2, 2, 3),
#'     to = c(2, 3, 4, 5, 5),
#'     values = round(rnorm(5, 5), 2))
#'
#' # Join the data frame to the graph's internal
#' # edge data frame (edf)
#' graph <- graph %>% join_edge_attrs(df)
#'
#' get_node_df(graph)
#' #>   id type label
#' #> 1  1    a   asd
#' #> 2  2    a  iekd
#' #> 3  3    b   idj
#' #> 4  4    b   edl
#' #> 5  5    b   ohd
#'
#' get_edge_df(graph)
#' #>   id from to  rel values
#' #> 1  1    1  2 <NA>   3.83
#' #> 2  2    1  3    A   4.47
#' #> 3  3    2  4    B   5.00
#' #> 4  4    2  5    C   4.49
#' #> 5  5    3  5    D   6.24
#'
#' # Perform a simple traversal from nodes to
#' # outbound edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge() %>%
#'   get_selection()
#' #> [1] "1 -> 2" "1 -> 3"
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge(
#'     conditions = "is.na(rel)") %>%
#'   get_selection()
#' #> [1] "1 -> 2"
#'
#' # Traverse from node `3` to any outbound
#' # edges, filtering to those edges that have
#' # numeric values greater than `5.0` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(3) %>%
#'   trav_out_edge(
#'     conditions = "values > 5.0") %>%
#'   get_selection()
#' #> [1] "1 -> 2"
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that
#' # have values equal to `A` for the `rel`
#' # edge attribute
#' graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge(
#'     conditions = "rel == 'A'") %>%
#'   get_selection()
#' #> [1] "1 -> 3"
#'
#' # Traverse from node `2` to any outbound
#' # edges, filtering to those edges that
#' # have values in the set `B` and `C` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_out_edge(
#'     conditions = "rel %in% c('B', 'C')") %>%
#'   get_selection()
#' #> [1] "2 -> 4" "2 -> 5"
#'
#' # Traverse from node `2` to any outbound
#' # edges, and use multiple conditions for the
#' # traversal (using a vector in `conditions`
#' # creates a set of `AND` conditions)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_out_edge(
#'     conditions = c(
#'       "rel %in% c('B', 'C')",
#'       "values >= 5.0")) %>%
#'   get_selection()
#' #> [1] "2 -> 4"
#'
#' # Traverse from node `2` to any outbound
#' # edges, and use multiple conditions with
#' # a single-length vector (here, using a
#' # `|` to create a set of `OR` conditions)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_out_edge(
#'     conditions = c(
#'       "rel %in% c('B', 'C') | values > 6.0")) %>%
#'   get_selection()
#' #> [1] "2 -> 4" "2 -> 5"
#'
#' # Traverse from node `2` to any outbound
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_out_edge(
#'     conditions = "grepl('B|C', rel)") %>%
#'   get_selection()
#' #> [1] "2 -> 4" "2 -> 5"
#' @importFrom dplyr filter filter_ select select_ full_join rename everything
#' @export trav_out_edge

trav_out_edge <- function(graph,
                          conditions = NULL,
                          copy_attrs_from = NULL) {

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
  starting_nodes <- graph$selection$nodes

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
  if (!is.null(conditions)) {
    for (i in 1:length(conditions)) {

      valid_edges <-
        valid_edges %>%
        dplyr::filter_(conditions[i])
    }
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
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = id.y) %>%
        dplyr::select(id, from, to, rel, dplyr::everything())
    }

    # If node attribute exists as a column in the edf
    if (copy_attrs_from %in% colnames(edf)) {

      edges <-
        ndf %>%
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = id.y)

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

  # Remove the node selection in graph
  graph$selection$nodes <- NULL

  # Update edge selection in graph
  graph$selection$edges$from <- valid_edges$from
  graph$selection$edges$to <- valid_edges$to

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

  return(graph)
}
