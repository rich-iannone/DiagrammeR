#' Traverse from one or more selected nodes onto
#' adjacent edges
#' @description From a graph object of class
#' \code{dgr_graph} move to adjacent edges from a
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
#' @param agg if a node attribute is provided
#' to \code{copy_attrs_from}, then an aggregation
#' function is required since there may be cases where
#' multiple node attribute values will be passed onto
#' the traversed edge(s). To pass only a single value,
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
#' get_edge_df(graph)
#' #>   id from to  rel values
#' #> 1  1    1  2 <NA>   6.00
#' #> 2  2    1  3    A   6.11
#' #> 3  3    2  4    B   4.72
#' #> 4  4    2  5    C   6.02
#' #> 5  5    3  5    D   5.05
#'
#' # Perform a simple traversal from nodes to
#' # adjacent edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(3) %>%
#'   trav_both_edge %>%
#'   get_selection()
#' #> [1] 2 5
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "is.na(rel)") %>%
#'   get_selection()
#' #> [1] 1
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that have
#' # numeric values greater than `6.5` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "values > 6.5") %>%
#'   get_selection()
#' #> [1] 2
#'
#' # Traverse from node `5` to any adjacent
#' # edges, filtering to those edges that
#' # have values equal to `C` for the `rel`
#' # edge attribute
#' graph %>%
#'   select_nodes_by_id(5) %>%
#'   trav_both_edge(
#'     conditions = "rel == 'C'") %>%
#'   get_selection()
#' #> [1] 4
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that
#' # have values in the set `B` and `C` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "rel %in% c('B', 'C')") %>%
#'   get_selection()
#' #> [1] 3 4
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use multiple conditions for the
#' # traversal (using a vector in `conditions`
#' # creates a set of `AND` conditions)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = c(
#'       "rel %in% c('B', 'C')",
#'       "values > 4.0")) %>%
#'   get_selection()
#' #> [1] 3 4
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use multiple conditions with
#' # a single-length vector (here, using a
#' # `|` to create a set of `OR` conditions)
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = c(
#'       "rel %in% c('B', 'C') | values > 4.0")) %>%
#'   get_selection()
#' #> [1] 1 3 4
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "grepl('B|C', rel)") %>%
#'   get_selection()
#' #> [1] 3 4
#' @importFrom dplyr filter filter_ select select_ full_join rename everything coalesce bind_cols
#' @importFrom tibble as_tibble
#' @export trav_both_edge

trav_both_edge <- function(graph,
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
  from <- to <- id <- id.y <- rel <- NULL

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
        dplyr::rename(from = id)

      edges <-
        ndf %>%
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edges, c("id" = "to")) %>%
        dplyr::rename(to = id.y) %>%
        dplyr::select(id, from, to, rel, dplyr::everything())

      # Get the column numbers for the `.x`
      # and `.y` columns
      x_col <- which(grepl("\\.x$", colnames(edges)))
      y_col <- which(grepl("\\.y$", colnames(edges)))

      # Coalesce the 2 generated columns and create a
      # single-column data frame
      value_col <-
        dplyr::coalesce(edges[, x_col], edges[, y_col]) %>%
        as.data.frame(stringsAsFactors = FALSE)

      # Rename the column
      colnames(value_col)[1] <- copy_attrs_from

      # Remove column numbers that end with ".x" or ".y"
      edges <- edges[-which(grepl("\\.x$", colnames(edges)))]
      edges <-edges[-which(grepl("\\.y$", colnames(edges)))]

      # Bind the `value_col` df to the `edges` df
      edges <- dplyr::bind_cols(edges, value_col)
    }

    # If node attribute exists as a column in the edf
    if (copy_attrs_from %in% colnames(edf)) {

      # Perform the first join
      edges <-
        ndf %>%
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = id.y)

      # Get the column numbers for the new columns
      x_col <- which(grepl("\\.x$", colnames(edges)))
      y_col <- which(grepl("\\.y$", colnames(edges)))

      # Coalesce the 2 generated columns
      value_col <-
        dplyr::coalesce(edges[, x_col], edges[, y_col]) %>%
        tibble::as_tibble()

      # Bind the `value_col` to the `edges` df
      edges <-
        edges %>%
        dplyr::bind_cols(value_col)

      # Remove column numbers that end with ".x" or ".y"
      edges <- edges[-which(grepl("\\.x$", colnames(edges)))]
      edges <- edges[-which(grepl("\\.y$", colnames(edges)))]

      # Rename the last column
      colnames(edges)[which(colnames(edges) == "value")] <-
        copy_attrs_from

      # Perform the second join
      edges <-
        ndf %>%
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edges, c("id" = "to")) %>%
        dplyr::rename(to = id)

      # Get the column numbers for the new columns
      x_col <- which(grepl("\\.x$", colnames(edges)))
      y_col <- which(grepl("\\.y$", colnames(edges)))

      # Coalesce the 2 generated columns
      value_col <-
        dplyr::coalesce(edges[, x_col], edges[, y_col]) %>%
        tibble::as_tibble()

      # Bind the `value_col` to the `edges` df
      edges <-
        edges %>%
        dplyr::bind_cols(value_col)

      # Remove column numbers that end with ".x" or ".y"
      edges <- edges[-which(grepl("\\.x$", colnames(edges)))]
      edges <- edges[-which(grepl("\\.y$", colnames(edges)))]

      # Rename the last column
      colnames(edges)[which(colnames(edges) == "value")] <-
        copy_attrs_from

      # Reorder columns
      edges <-
        edges %>%
        dplyr::select(id, from, to, rel, dplyr::everything())
    }

    # Update the graph's internal node data frame
    graph$edges_df <- edges
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
      function_used = "trav_both_edge",
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
