#' Traverse from one or more selected nodes onto
#' adjacent edges
#' @description From a graph object of class
#' \code{dgr_graph} move to adjacent edges from a
#' selection of one or more selected nodes, thereby
#' creating a selection of edges. An optional filter
#' by edge attribute can limit the set of edges
#' traversed to.
#' @param graph a graph object of class \code{dgr_graph}
#' that is created using \code{create_graph}.
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
#' get_edge_df(graph)
#' #>   from to  rel values
#' #> 1    1  2 <NA>   5.19
#' #> 2    1  3    A   4.57
#' #> 3    2  4    B   5.91
#' #> 4    2  5    C   6.79
#' #> 5    3  5    D      6
#'
#' # Perform a simple traversal from nodes to
#' # adjacent edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(3) %>%
#'   trav_both_edge %>%
#'   get_selection
#' #> [1] "1 -> 3" "3 -> 5"
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "is.na(rel)") %>%
#'   get_selection
#' #> [1] "1 -> 2"
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that have
#' # numeric values greater than `6.5` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "values > 6.5") %>%
#'   get_selection
#' #> [1] "2 -> 5"
#'
#' # Traverse from node `5` to any adjacent
#' # edges, filtering to those edges that
#' # have values equal to `C` for the `rel`
#' # edge attribute
#' graph %>%
#'   select_nodes_by_id(5) %>%
#'   trav_both_edge(
#'     conditions = "rel == 'C'") %>%
#'   get_selection
#' #> [1] "2 -> 5"
#'
#' # Traverse from node `2` to any adjacent
#' # edges, filtering to those edges that
#' # have values in the set `B` and `C` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "rel %in% c('B', 'C')") %>%
#'   get_selection
#' #> [1] "2 -> 4" "2 -> 5"
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
#'   get_selection
#' #> [1] "2 -> 4" "2 -> 5"
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
#'   get_selection
#' #> [1] "1 -> 2" "2 -> 4" "2 -> 5"
#'
#' # Traverse from node `2` to any adjacent
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_both_edge(
#'     conditions = "grepl('B|C', rel)") %>%
#'   get_selection
#' #> [1] "2 -> 4" "2 -> 5"
#' @importFrom dplyr filter filter_ select select_ full_join rename everything coalesce bind_cols
#' @importFrom tibble as_tibble
#' @export trav_both_edge

trav_both_edge <- function(graph,
                           conditions = NULL,
                           copy_attrs_from = NULL) {

  if (is.null(graph$selection$nodes)) {
    stop("There is no selection of nodes available.")
  }

  # Add bindings for variables
  from <- to <- NULL

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_nodes <- graph$selection$nodes

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
        dplyr::rename(to = id) %>%
        dplyr::select(from, to, rel, dplyr::everything())
    }

    # If node attribute exists as a column in the edf
    if (copy_attrs_from %in% colnames(edf)) {

      # Perform the first join
      edges <-
        ndf %>%
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edf, c("id" = "from")) %>%
        dplyr::rename(from = id)

      # Coalesce the 2 generated columns
      value_col <-
        dplyr::coalesce(edges$value.x, edges$value.y) %>%
        tibble::as_tibble()

      # Bind the `value_col` to the `edges` df
      edges <-
        edges %>%
        dplyr::bind_cols(value_col)

      # Remove column numbers that end with ".x" or ".y"
      edges <-
        edges[-which(grepl("\\.x$", colnames(edges)))]

      edges <-
        edges[-which(grepl("\\.y$", colnames(edges)))]

      # Perform the second join
      edges <-
        ndf %>%
        dplyr::filter(id == starting_nodes) %>%
        dplyr::select_("id", copy_attrs_from) %>%
        dplyr::full_join(edges, c("id" = "to")) %>%
        dplyr::rename(to = id)

      # Coalesce the 2 generated columns
      value_col <-
        dplyr::coalesce(edges$value.x, edges$value.y) %>%
        tibble::as_tibble()

      # Bind the `value_col` to the `edges` df
      edges <-
        edges %>%
        dplyr::bind_cols(value_col)

      # Remove column numbers that end with ".x" or ".y"
      edges <-
        edges[-which(grepl("\\.x$", colnames(edges)))]

      edges <-
        edges[-which(grepl("\\.y$", colnames(edges)))]

      # Reorder columns
      edges <-
        edges %>%
        dplyr::select(from, to, rel, dplyr::everything())
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

  # Remove the node selection in graph
  graph$selection$nodes <- NULL

  # Update edge selection in graph
  graph$selection$edges$from <- valid_edges$from
  graph$selection$edges$to <- valid_edges$to

  return(graph)
}
