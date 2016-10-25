#' Traverse from one or more selected nodes onto
#' adjacent, outward edges
#' @description From a graph object of class
#' \code{dgr_graph} move to outgoing edges from a
#' selection of one or more selected nodes, thereby
#' creating a selection of edges. An optional filter
#' by edge attribute can limit the set of edges
#' traversed to.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr an optional character vector of
#' edge attribute values for filtering the node ID
#' values returned.
#' @param conditions an option to use filtering
#' conditions for the traversal.
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
#' # outbound edges with no conditions on the
#' # nodes traversed to
#' graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge %>%
#'   get_selection
#' #> [1] "1 -> 2" "1 -> 3"
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that have
#' # NA values for the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge(
#'     conditions = "is.na(rel)") %>%
#'   get_selection
#' #> [1] "1 -> 2"
#'
#' # Traverse from node `1` to any outbound
#' # edges, filtering to those edges that have
#' # numeric values greater than `5.0` for
#' # the `rel` edge attribute
#' graph %>%
#'   select_nodes_by_id(1) %>%
#'   trav_out_edge(
#'     conditions = "values > 5.0") %>%
#'   get_selection
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
#'   get_selection
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
#'   get_selection
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
#'       "values > 6.0")) %>%
#'   get_selection
#' #> [1] "2 -> 5"
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
#'   get_selection
#' #> [1] "2 -> 4" "2 -> 5"
#'
#' # Traverse from node `2` to any outbound
#' # edges, and use a regular expression as
#' # a filtering condition
#' graph %>%
#'   select_nodes_by_id(2) %>%
#'   trav_out_edge(
#'     conditions = "grepl('B|C', rel)") %>%
#'   get_selection
#' #> [1] "2 -> 4" "2 -> 5"
#' @importFrom dplyr filter filter_
#' @export trav_out_edge

trav_out_edge <- function(graph,
                          conditions = NULL) {

  if (is.null(graph$selection$nodes)) {
    stop("There is no selection of nodes available.")
  }

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_nodes <- graph$selection$nodes

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

  # Remove the node selection in graph
  graph$selection$nodes <- NULL

  # Update edge selection in graph
  graph$selection$edges$from <- valid_edges$from
  graph$selection$edges$to <- valid_edges$to

  return(graph)
}
