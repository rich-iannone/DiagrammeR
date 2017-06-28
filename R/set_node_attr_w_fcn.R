#' Set node attribute values with a graph function
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, set node
#' attribute properties for all nodes in the graph
#' using one of several whole-graph functions.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param fcn the name of the function to use.
#' @param ... arguments and values to pass to
#' the named function in \code{fcn}, if necessary.
#' @param column_name an option to supply a column
#' name for the new node attribute column. If
#' \code{NULL} then the column name supplied by the
#' function will used along with a \code{__A}
#' suffix.
#' @return either a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 10, m = 22,
#'     set_seed = 23)
#'
#' # Get the betweenness values for
#' # each of the graph's nodes as a
#' # node attribute
#' graph_1 <-
#'   graph %>%
#'   set_node_attr_w_fcn(
#'     fcn = "get_betweenness")
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_1 %>%
#'   get_node_df()
#' #>    id type label value betweenness__A
#' #> 1   1 <NA>     1   6.0       5.904762
#' #> 2   2 <NA>     2   2.5       4.904762
#' #> 3   3 <NA>     3   3.5       1.785714
#' #> 4   4 <NA>     4   7.5       0.000000
#' #> 5   5 <NA>     5   8.5       5.738095
#' #> 6   6 <NA>     6   4.5      20.523810
#' #> 7   7 <NA>     7  10.0       3.333333
#' #> 8   8 <NA>     8  10.0       0.000000
#' #> 9   9 <NA>     9   8.5       3.738095
#' #> 10 10 <NA>    10  10.0       4.071429
#'
#' # If a specified function takes argument
#' # values, these can be supplied as well
#' graph_2 <-
#'   graph %>%
#'   set_node_attr_w_fcn(
#'     fcn = "get_alpha_centrality",
#'     alpha = 2,
#'     exo = 2)
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_2 %>%
#'   get_node_df()
#' #>    id type label value alpha_centrality__A
#' #> 1   1 <NA>     1   6.0                   2
#' #> 2   2 <NA>     2   2.5                   2
#' #> 3   3 <NA>     3   3.5                   6
#' #> 4   4 <NA>     4   7.5                   2
#' #> 5   5 <NA>     5   8.5                  14
#' #> 6   6 <NA>     6   4.5                  50
#' #> 7   7 <NA>     7  10.0                  22
#' #> 8   8 <NA>     8  10.0                 106
#' #> 9   9 <NA>     9   8.5                 162
#' #> 10 10 <NA>    10  10.0                 462
#'
#' graph_3 <-
#'   graph %>%
#'   set_node_attr_w_fcn(
#'     fcn = "get_s_connected_cmpts",
#'     column_name = "scc")
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_3 %>%
#'   get_node_df()
#' #>    id type label value scc
#' #> 1   1 <NA>     1   6.0   4
#' #> 2   2 <NA>     2   2.5   2
#' #> 3   3 <NA>     3   3.5   3
#' #> 4   4 <NA>     4   7.5   1
#' #> 5   5 <NA>     5   8.5   6
#' #> 6   6 <NA>     6   4.5   7
#' #> 7   7 <NA>     7  10.0   5
#' #> 8   8 <NA>     8  10.0  10
#' #> 9   9 <NA>     9   8.5   8
#' #> 10 10 <NA>    10  10.0   9
#' @importFrom dplyr inner_join mutate
#' @export set_node_attr_w_fcn

set_node_attr_w_fcn <- function(graph,
                                fcn,
                                ...,
                                column_name = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Create bindings for specific variables
  id <- NULL

  if (!(fcn %in% (value_per_node_functions() %>% names()))) {
    stop("The function name must be one that produces values for every graph node.")
  }

  # Collect extra vectors of arguments and values as `extras`
  extras <- list(...)

  if (length(extras) > 0) {

  nodes_df <-
    graph$nodes_df %>%
    dplyr::inner_join(
      eval(
        parse(
          text = paste0(
            fcn,
            "(graph, ",
            paste(names(extras),
                  "=",
                  extras,
                  collapse =  ", "),
            ")"))) %>%
        dplyr::mutate(id = as.integer(id)),
      by = "id")

  } else {

    nodes_df <-
      graph$nodes_df %>%
      dplyr::inner_join(
        eval(
          parse(
            text = paste0(
              fcn,
              "(graph)"))) %>%
          dplyr::mutate(id = as.integer(id)),
        by = "id")
  }

  if (!is.null(column_name)) {

    # If a new column name is specified in
    # `column_name`, use that for the new column
    colnames(nodes_df)[length(colnames(nodes_df))] <-
      column_name
  } else {

    # Add the `_A` tag to the column name normally
    # supplied by the function
    colnames(nodes_df)[length(colnames(nodes_df))] <-
      paste0(colnames(nodes_df)[length(colnames(nodes_df))], "__A")
  }

  # Replace the graph's ndf with the
  # revised version
  graph$nodes_df <- nodes_df

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_node_attr_w_fcn",
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
