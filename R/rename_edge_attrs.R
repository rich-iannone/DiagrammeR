#' Rename an edge attribute
#' @description Within a graph's internal edge data
#' frame (edf), rename an existing edge attribute.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr_from the name of the edge attribute
#' that will be renamed.
#' @param edge_attr_to the new name of the edge
#' attribute column identified in \code{edge_attr_from}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'   5, 8, set_seed = 3) %>%
#'   set_edge_attrs("color", "green")
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   from to  rel color
#' #> 1    4  1 <NA> green
#' #> 2    2  4 <NA> green
#' #> 3    4  3 <NA> green
#' #> 4    3  5 <NA> green
#' #> 5    5  1 <NA> green
#' #> 6    4  5 <NA> green
#' #> 7    2  1 <NA> green
#' #> 8    3  2 <NA> green
#'
#' # Rename the `value` node attribute as `weight`
#' graph <-
#'   graph %>%
#'   rename_edge_attrs("color", "labelfontcolor")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute had been renamed
#' get_edge_df(graph)
#' #>   from to  rel labelfontcolor
#' #> 1    4  1 <NA>          green
#' #> 2    2  4 <NA>          green
#' #> 3    4  3 <NA>          green
#' #> 4    3  5 <NA>          green
#' #> 5    5  1 <NA>          green
#' #> 6    4  5 <NA>          green
#' #> 7    2  1 <NA>          green
#' #> 8    3  2 <NA>          green
#' @export rename_edge_attrs

rename_edge_attrs <- function(graph,
                              edge_attr_from,
                              edge_attr_to) {

  # Stop function if `edge_attr_from` and
  # `edge_attr_to` are identical
  if (edge_attr_from == edge_attr_to) {
    stop("You cannot rename using the same name.")
  }

  # Stop function if `edge_attr_to` is `from`, `to`,
  # or any other column name in the graph's edge
  # data frame
  if (any(colnames(get_edge_df(graph)) %in%
          edge_attr_to)) {
    stop("You cannot use that name for `edge_attr_to`.")
  }

  # Stop function if `edge_attr_from` is `from`, `to`
  # or `rel`
  if (any(c("from", "to", "rel") %in%
          edge_attr_from)) {
    stop("You cannot use that name for `edge_attr_from`.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's columns
  if (!any(column_names_graph %in% edge_attr_from)) {
    stop("The edge attribute to rename is not in the edf.")
  }

  # Set the column name for the renamed attr
  colnames(edges)[
    which(colnames(edges) %in%
            edge_attr_from)] <- edge_attr_to

  # Modify the graph's edf
  graph$edges_df <- edges

  # Update the `last_node` counter
  graph$last_node <- nodes_created

  return(graph)
}
