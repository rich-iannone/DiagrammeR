#' Set edge attributes with an edge selection
#' @description From a graph object of class
#' \code{dgr_graph} or an edge data frame, set edge
#' attribute properties for one or more edges
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr the name of the attribute to set.
#' @param value the value to be set for the chosen
#' attribute for the edges in the current selection.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_path(6)
#'
#' # Select specific edges from the graph and
#' # apply the edge attribute `color = blue` to
#' # those selected edges
#' graph <-
#'   graph %>%
#'   select_nodes_by_id(2:4) %>%
#'   trav_out_edge %>%
#'   set_edge_attrs_ws("color", "blue")
#'
#' # Show the internal edge data frame to verify
#' # that the edge attribute has been set for
#' # specific edges
#' get_edge_df(graph)
#' #>   from to rel color
#' #> 1    1  2
#' #> 2    2  3      blue
#' #> 3    3  4      blue
#' #> 4    4  5      blue
#' #> 5    5  6
#' @export set_edge_attrs_ws

set_edge_attrs_ws <- function(graph,
                              edge_attr,
                              value) {

  edges_df <- graph$edges_df
  from <- graph$selection$edges$from
  to <- graph$selection$edges$to

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  if (edge_attr %in% colnames(edges_df)) {
    if (is.null(from) & !is.null(to)) {
      edges_df[which(edges_df$to %in% to),
               which(colnames(edges_df) %in%
                       edge_attr)] <- value
    } else if (!is.null(from) & is.null(to)) {
      edges_df[which(edges_df$from %in% from),
               which(colnames(edges_df) %in%
                       edge_attr)] <- value
    } else if (is.null(from) & is.null(to)) {
      edges_df[, which(colnames(edges_df) %in%
                         edge_attr)] <- value
    } else {
      edf_vector <-
        paste0(edges_df$from, " -> ", edges_df$to)
      sel_vector <- get_selection(graph)

      edges_df[which(edf_vector %in% sel_vector),
               which(colnames(edges_df) %in%
                       edge_attr)] <- value
    }
  }

  if (!(edge_attr %in% colnames(edges_df))) {
    edges_df <-
      cbind(edges_df, rep("", nrow(edges_df)))
    edges_df[, ncol(edges_df)] <-
      as.character(edges_df[, ncol(edges_df)])
    colnames(edges_df)[ncol(edges_df)] <- edge_attr

    if (is.null(from) & !is.null(to)) {
      edges_df[which(edges_df$to %in% to),
               ncol(edges_df)] <- value
    } else if (!is.null(from) & is.null(to)) {
      edges_df[which(edges_df$from %in% from),
               ncol(edges_df)] <- value
    } else if (is.null(from) & is.null(to)) {
      edges_df[, ncol(edges_df)] <- value
    } else {
      edf_vector <-
        paste0(edges_df$from, " -> ", edges_df$to)
      sel_vector <- get_selection(graph)

      edges_df[which(edf_vector %in% sel_vector),
               ncol(edges_df)] <- value
    }
  }

  # Create new graph object
  dgr_graph <-
    create_graph(
      nodes_df = graph$nodes_df,
      edges_df = edges_df,
      graph_attrs = graph$graph_attrs,
      node_attrs = graph$node_attrs,
      edge_attrs = graph$edge_attrs,
      directed = ifelse(is_graph_directed(graph),
                        TRUE, FALSE),
      graph_name = graph$graph_name,
      graph_time = graph$graph_time,
      graph_tz = graph$graph_tz)

  # Retain the edge selection in the graph
  dgr_graph$selection <- graph$selection

  # Update the `last_node` counter
  dgr_graph$last_node <- nodes_created

  return(dgr_graph)
}
