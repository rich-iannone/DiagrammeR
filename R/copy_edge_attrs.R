#' Copy an edge attribute column and set the name
#' @description Within a graph's internal edge data
#' frame (edf), copy the contents an existing edge
#' attribute and create a distinct edge attribute
#' within the edf with a different attribute name.
#' @param graph a graph object of class
#' @param edge_attr_from the name of the edge attribute
#' column from which values will be copied.
#' @param edge_attr_to the name of the new edge
#' attribute column to which the copied values will be
#' placed.
#' @return a graph object of class
#' \code{dgr_graph}.
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
#' #>   from to rel color
#' #> 1    4  1     green
#' #> 2    2  4     green
#' #> 3    4  3     green
#' #> 4    3  5     green
#' #> 5    5  1     green
#' #> 6    4  5     green
#' #> 7    2  1     green
#' #> 8    3  2     green
#'
#' # Make a copy the `color` edge attribute as
#' # the `color_2` edge attribute
#' graph <-
#'   graph %>%
#'   copy_edge_attrs("color", "color_2")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute had been copied
#' get_edge_df(graph)
#' #>   from to rel color color_2
#' #> 1    4  1     green   green
#' #> 2    2  4     green   green
#' #> 3    4  3     green   green
#' #> 4    3  5     green   green
#' #> 5    5  1     green   green
#' #> 6    4  5     green   green
#' #> 7    2  1     green   green
#' #> 8    3  2     green   green
#' @export copy_edge_attrs

copy_edge_attrs <- function(graph,
                            edge_attr_from,
                            edge_attr_to) {

  # Stop function if `edge_attr_from` and
  # `edge_attr_to` are identical
  if (edge_attr_from == edge_attr_to) {
    stop("You cannot use make a copy with the same name.")
  }

  # Stop function if `edge_attr_to` is `from` or `to`
  if (any(c("from", "to") %in% edge_attr_to)) {
    stop("You cannot use those names.")
  }

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's column
  if (!any(column_names_graph %in% edge_attr_from)) {
    stop("The edge attribute to copy is not in the ndf.")
  }

  # Get the column number for the edge attr to copy
  col_num_copy_from <-
    which(colnames(edges) %in% edge_attr_from)

  # Copy the column through a `cbind()`
  edges <- cbind(edges, edges[,col_num_copy_from])

  # Set the column name for the copied attr
  colnames(edges)[ncol(edges)] <- edge_attr_to

  # Create a new graph object
  dgr_graph <-
    create_graph(
      nodes_df = graph$nodess_df,
      edges_df = edges,
      graph_attrs = graph$graph_attrs,
      node_attrs = graph$node_attrs,
      edge_attrs = graph$edge_attrs,
      directed = graph$directed,
      graph_name = graph$graph_name,
      graph_time = graph$graph_time,
      graph_tz = graph$graph_tz)

  # Update the `last_node` counter
  dgr_graph$last_node <- nodes_created

  return(dgr_graph)
}
