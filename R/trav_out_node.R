#' Traverse from one or more selected edges onto
#' adjacent, outward nodes
#' @description From a graph object of class
#' \code{dgr_graph} with an active selection of
#' edges move opposite to the edge direction to
#' connected nodes, replacing the current edge selection
#' with the selection with those nodes traversed to. An
#' optional filter by node attribute can limit the set
#' of nodes traversed to.
#' @param graph a graph object of class \code{dgr_graph}
#' that is created using \code{create_graph}.
#' @param conditions an option to use filtering
#' conditions for the traversal.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(dplyr)
#'
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
#' #> 1  1    a   asd   8.11
#' #> 2  2    a  iekd   6.72
#' #> 3  3    b   idj   8.02
#' #> 4  4    b   edl   7.05
#' #> 5  5    b   ohd   8.58
#'
#' get_edge_df(graph)
#' #>   from to  rel values
#' #> 1    1  2 <NA>   5.19
#' #> 2    1  3    A   4.57
#' #> 3    2  4    B   5.91
#' #> 4    2  5    C   6.79
#' #> 5    3  5    D      6
#'
#' # Perform a simple traversal from the
#' # edge `1` -> `3` to the attached node
#' # in the direction of the edge; here, no
#' # conditions are placed on the nodes
#' # traversed to
#' graph %>%
#'   select_edges(from = 1, to = 3) %>%
#'   trav_out_node %>%
#'   get_selection
#' #> [1] 1
#'
#' # Traverse from edges `2` -> `5` and
#' # `3` -> `5` to the attached node along
#' # the direction of the edge; here, the
#' # traversals lead to different nodes
#' graph %>%
#'   select_edges(from = 2, to = 5) %>%
#'   select_edges(from = 3, to = 5) %>%
#'   trav_out_node %>%
#'   get_selection
#' #> [1] 2 3
#'
#' # Traverse from the edge `1` -> `3`
#' # to the attached node where the edge
#' # is outgoing, this time filtering
#' # numeric values greater than `7.0` for
#' # the `values` node attribute
#' graph %>%
#'   select_edges(from = 1, to = 3) %>%
#'   trav_out_node(
#'     conditions = "values > 7.0") %>%
#'   get_selection
#' #> [1] 1
#'
#' # Traverse from the edge `1` -> `3`
#' # to the attached node where the edge
#' # is outgoing, this time filtering
#' # numeric values less than `7.0` for
#' # the `values` node attribute (the
#' # condition is not met so the original
#' # selection of edge `1` -> `3` remains)
#' graph %>%
#'   select_edges(from = 1, to = 3) %>%
#'   trav_out_node(
#'     conditions = "values < 7.0") %>%
#'   get_selection
#' #> [1] "1 -> 3"
#'
#' # Traverse from the edge `1` -> `2` to
#' # the node `2` using multiple conditions
#' # with a single-length vector (here, using
#' # a `|` to create a set of `OR` conditions)
#' graph %>%
#'   select_edges(from = 1, to = 2) %>%
#'   trav_out_node(
#'     conditions = "grepl('.*d$', label) | values < 6.0") %>%
#'   get_selection
#' #> [1] 1
#' @importFrom dplyr filter filter_
#' @export trav_out_node

trav_out_node <- function(graph,
                          conditions = NULL) {

  if (is.null(graph$selection$edges$from) |
      is.null(graph$selection$edges$to)) {
    stop("There is no selection of edges available.")
  }

  # Add binding
  id <- NULL

  # Get the selection of nodes as the starting
  # nodes for the traversal
  starting_edge_from <- graph$selection$edges$from
  starting_edge_to <- graph$selection$edges$to

  # Get the graph's node data frame
  ndf <- graph$nodes_df

  # 1 -> 3
  # Find all nodes that are connected to the
  # starting nodes via incoming edges
  valid_nodes <-
    ndf %>%
    dplyr::filter(id %in% starting_edge_from)

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

  # If no rows returned, then there are no
  # valid traversals, so return the same graph
  # object without modifying the selection
  if (nrow(valid_nodes) == 0) {
    return(graph)
  }

  # Remove the edge selection in graph
  graph$selection$edges$from <- NULL
  graph$selection$edges$to <- NULL
  graph$selection$edges <- NULL

  # Update the node selection in graph
  graph$selection$nodes <- valid_nodes$id

  return(graph)
}
