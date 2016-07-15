#' Join new node attribute values using a data frame
#' @description Join new node attribute values in a
#' left join using a data frame. The use of a left join
#' in this function allows for no possibility that
#' nodes in the graph might be removed after the join.
#' @param graph a graph object of class
#' @param df the data frame to use for joining.
#' @param by_graph optional specification of the column
#' in the graph's internal node data frame for the left
#' join. If both \code{by_graph} and \code{by_df} are
#' not provided, then a natural join will occur if
#' there are columns in the graph's ndf and in
#' \code{df} with identical names.
#' @param by_df optional specification of the column in
#' \code{df} for the left join. If both \code{by_graph}
#' and \code{by_df} are not provided, then a natural
#' join will occur if there are columns in the graph's
#' ndf and in \code{df} with identical names.
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
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
#' # Create a data frame with node ID values and a
#' # set of numeric values
#' df <- data.frame(nodes = 1:6, values = rnorm(6, 5))
#'
#' # Join the values in the data frame to the
#' # graph's nodes; this works as a left join using
#' # identically-named columns in the graph and the df
#' # (in this case `nodes` is common to both)
#' graph <-
#'   graph %>% join_node_attrs(df)
#'
#' # Get the graph's internal ndf to show that the
#' # join has been made
#' get_node_df(graph)
#' #>   nodes type label           values
#' #> 1     1            4.27988818205506
#' #> 2     2             5.3499594040959
#' #> 3     3            5.43965531750004
#' #> 4     4            3.50233363164518
#' #> 5     5            5.04599475422798
#' @export join_node_attrs

join_node_attrs <- function(graph,
                            df,
                            by_graph = NULL,
                            by_df = NULL) {

  if (is.null(by_graph) & !is.null(by_df)) {
    stop("Both column specifications must be provided.")
  }

  if (!is.null(by_graph) & is.null(by_df)) {
    stop("Both column specifications must be provided.")
  }

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Get column names from the df
  column_names_df <- colnames(df)

  if (is.null(by_graph) & is.null(by_df)) {

    # Perform a left join on the `nodes` data frame
    nodes <- merge(nodes, df, all.x = TRUE)
  }

  if (!is.null(by_graph) & !is.null(by_df)) {

    # Perform a left join on the `nodes` data frame
    nodes <-
      merge(nodes, df,
            all.x = TRUE,
            by.x = by_graph,
            by.y = by_df)
  }

  # Get new column names in the revised ndf
  new_col_names <-
    setdiff(colnames(nodes), column_names_graph)

  # Get the column numbers for the new columns
  col_numbers <-
    which(colnames(nodes) %in% new_col_names)

  # Replace string <NA> values with empty strings
  for (i in 1:length(col_numbers)) {
    nodes[,col_numbers[i]][
      is.na(nodes[,col_numbers[i]])] <- ""
  }

  # Create a new graph object
  dgr_graph <-
    create_graph(nodes_df = nodes,
                 edges_df = graph$edges_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
