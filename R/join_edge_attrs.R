#' Join new edge attribute values using a data frame
#' @description Join new edge attribute values in a
#' left join using a data frame. The use of a left join
#' in this function allows for no possibility that
#' edges in the graph might be removed after the join.
#' @param graph a graph object of class
#' @param df the data frame to use for joining.
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param by_graph optional specification of the column
#' in the graph's internal edge data frame for the left
#' join. If both \code{by_graph} and \code{by_df} are
#' not provided, then a natural join will occur if
#' there are columns in the graph's edf and in
#' \code{df} with identical names.
#' @param by_df optional specification of the column in
#' \code{df} for the left join. If both \code{by_graph}
#' and \code{by_df} are not provided, then a natural
#' join will occur if there are columns in the graph's
#' edf and in \code{df} with identical names.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(5) %>%
#'   add_edges_w_string("1->2 1->3 2->4 2->5 3->5")
#'
#' # Create a data frame with node ID values
#' # representing the graph edges (with `from` and `to`
#' # columns), and, a set of numeric values
#' df <-
#'   data.frame(from = c(1, 1, 2, 2, 3),
#'              to = c(2, 3, 4, 5, 5),
#'              values = rnorm(5, 5))
#'
#' # Join the values in the data frame to the
#' # graph's edges; this works as a left join using
#' # identically-named columns in the graph and the df
#' # (in this case `from` and `to` are common to both)
#' graph <-
#'   graph %>% join_edge_attrs(df)
#'
#' # Get the graph's internal edf to show that the
#' # join has been made
#' get_edge_df(graph)
#' #>   from to rel           values
#' #> 1    1  2     7.13009924330664
#' #> 2    1  3     4.91228554626235
#' #> 3    2  4     3.53310530960626
#' #> 4    2  5      5.6516614809259
#' #> 5    3  5     5.87955663602654
#' @export join_edge_attrs

join_edge_attrs <- function(graph,
                            df,
                            by_graph = NULL,
                            by_df = NULL) {

  if (is.null(by_graph) & !is.null(by_df)) {
    stop("Both column specifications must be provided.")
  }

  if (!is.null(by_graph) & is.null(by_df)) {
    stop("Both column specifications must be provided.")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names <- colnames(edges)

  if (is.null(by_graph) & is.null(by_df)) {

    # Perform a left join on the `edges` data frame
    edges <- merge(edges, df, all.x = TRUE)
  }

  if (!is.null(by_graph) & !is.null(by_df)) {

    # Perform a left join on the `edges` data frame
    edges <-
      merge(edges, df,
            all.x = TRUE,
            by.x = by_graph,
            by.y = by_df)
  }

  # Get new column names in the revised edf
  new_col_names <-
    setdiff(colnames(edges), column_names)

  # Get the column numbers for the new columns
  col_numbers <-
    which(colnames(edges) %in% new_col_names)

  # Replace string <NA> values with empty strings
  for (i in 1:length(col_numbers)) {
    edges[,col_numbers[i]][is.na(edges[,col_numbers[i]])] <- ""
  }

  # Create a new graph object
  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = edges,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
