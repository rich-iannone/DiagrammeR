#' Set node attribute values with a graph function
#' @description From a graph object of class
#' \code{dgr_graph} or a node data frame, set node
#' attribute properties for all nodes in the graph
#' using one of several whole-graph functions.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr_fcn the name of the function to
#' use for creating a column of node attribute values.
#' Valid functions are: \code{get_alpha_centrality},
#' \code{get_authority_centrality},
#' \code{get_betweenness}, \code{get_bridging},
#' \code{get_closeness}, \code{get_cmty_edge_btwns},
#' \code{get_cmty_fast_greedy}, \code{get_cmty_l_eigenvec},
#' \code{get_cmty_louvain}, \code{get_cmty_walktrap},
#' \code{get_constraint}, \code{get_degree_distribution},
#' \code{get_degree_histogram}, \code{get_degree_in},
#' \code{get_degree_out}, \code{get_degree_total},
#' \code{get_eccentricity}, \code{get_eigen_centrality},
#' \code{get_pagerank}, \code{get_s_connected_cmpts},
#' and \code{get_w_connected_cmpts}.
#' @param ... arguments and values to pass to
#' the named function in \code{node_attr_fcn}, if
#' necessary.
#' @param column_name an option to supply a column
#' name for the new node attribute column. If
#' \code{NULL} then the column name supplied by the
#' function will used along with a \code{__A}
#' suffix.
#' @return either a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 22,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = value,
#'     values = rnorm(
#'       n = count_nodes(.),
#'       mean = 5,
#'       sd = 1) %>% round(1))
#'
#' # Get the betweenness values for
#' # each of the graph's nodes as a
#' # node attribute
#' graph_1 <-
#'   graph %>%
#'   set_node_attr_w_fcn(
#'     node_attr_fcn = "get_betweenness")
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_1 %>%
#'   get_node_df()
#' #>    id type label betweenness__A
#' #> 1   1 <NA>  <NA>       9.333333
#' #> 2   2 <NA>  <NA>      29.000000
#' #> 3   3 <NA>  <NA>      19.166667
#' #> 4   4 <NA>  <NA>       2.666667
#' #> 5   5 <NA>  <NA>       0.500000
#' #> 6   6 <NA>  <NA>      18.000000
#' #> 7   7 <NA>  <NA>      12.000000
#' #> 8   8 <NA>  <NA>       0.000000
#' #> 9   9 <NA>  <NA>      10.333333
#' #> 10 10 <NA>  <NA>       0.000000
#'
#' # If a specified function takes argument
#' # values, these can be supplied as well
#' graph_2 <-
#'   graph %>%
#'   set_node_attr_w_fcn(
#'     node_attr_fcn = "get_alpha_centrality",
#'     alpha = 2,
#'     exo = 2)
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_2 %>%
#'   get_node_df()
#' #>    id type label alpha_centrality__A
#' #> 1   1 <NA>  <NA>           0.0621118
#' #> 2   2 <NA>  <NA>          -0.5341615
#' #> 3   3 <NA>  <NA>          -0.8157350
#' #> 4   4 <NA>  <NA>          -0.6997930
#' #> 5   5 <NA>  <NA>           1.0641822
#' #> 6   6 <NA>  <NA>          -0.8737060
#' #> 7   7 <NA>  <NA>          -0.6832298
#' #> 8   8 <NA>  <NA>           0.9316770
#' #> 9   9 <NA>  <NA>          -0.4679089
#' #> 10 10 <NA>  <NA>           0.3685300
#'
#' # The new column name can be provided
#' graph_3 <-
#'   graph %>%
#'   set_node_attr_w_fcn(
#'     node_attr_fcn = "get_pagerank",
#'     column_name = "pagerank")
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_3 %>%
#'   get_node_df()
#' #>    id type label pagerank
#' #> 1   1 <NA>  <NA>   0.1416
#' #> 2   2 <NA>  <NA>   0.1401
#' #> 3   3 <NA>  <NA>   0.1262
#' #> 4   4 <NA>  <NA>   0.0637
#' #> 5   5 <NA>  <NA>   0.0478
#' #> 6   6 <NA>  <NA>   0.1976
#' #> 7   7 <NA>  <NA>   0.1318
#' #> 8   8 <NA>  <NA>   0.0422
#' #> 9   9 <NA>  <NA>   0.0693
#' #> 10 10 <NA>  <NA>   0.0398
#'
#' # If `graph_3` is modified by
#' # adding a new node then the column
#' # `pagerank` will have stale data; we
#' # can run the function again and re-use
#' # the existing column name to provide
#' # updated values
#' graph_3 <-
#'   graph_3 %>%
#'   add_node(
#'     from = 1,
#'     to = 3) %>%
#'   set_node_attr_w_fcn(
#'     node_attr_fcn = "get_pagerank",
#'     column_name = "pagerank")
#'
#' # Inspect the graph's internal
#' # node data frame
#' graph_3 %>%
#'   get_node_df()
#' #>    id type label pagerank
#' #> 1   1 <NA>  <NA>   0.1349
#' #> 2   2 <NA>  <NA>   0.1352
#' #> 3   3 <NA>  <NA>   0.1585
#' #> 4   4 <NA>  <NA>   0.0670
#' #> 5   5 <NA>  <NA>   0.0461
#' #> 6   6 <NA>  <NA>   0.1300
#' #> 7   7 <NA>  <NA>   0.1014
#' #> 8   8 <NA>  <NA>   0.0400
#' #> 9   9 <NA>  <NA>   0.0685
#' #> 10 10 <NA>  <NA>   0.0440
#' #> 11 11 <NA>  <NA>   0.0744
#' @importFrom dplyr inner_join mutate
#' @export set_node_attr_w_fcn

set_node_attr_w_fcn <- function(graph,
                                node_attr_fcn,
                                ...,
                                column_name = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  id <- NULL

  value_per_node_fcn_names <-
    value_per_node_functions() %>% names()

  if (!any(value_per_node_fcn_names %in% node_attr_fcn)) {

    stop(
      "The function name must be one that produces values for every graph node.",
      call. = FALSE)
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
            node_attr_fcn,
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
              node_attr_fcn,
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

  # Determine if there is a column with the same name;
  # if there is, replace its contents with that of the
  # new column
  if (colnames(nodes_df)[length(colnames(nodes_df))] %in%
      colnames(nodes_df)[1:(length(colnames(nodes_df)) - 1)]) {

    col_no_matching <-
      which(colnames(nodes_df)[1:(length(colnames(nodes_df)) - 1)] ==
              colnames(nodes_df)[length(colnames(nodes_df))])

    # Move contents of new column to matching column
    nodes_df[, col_no_matching] <-
      nodes_df[, length(colnames(nodes_df))]

    # Remove the last column from the ndf
    nodes_df[, length(colnames(nodes_df))] <- NULL
  }

  if (paste0(colnames(nodes_df)[length(colnames(nodes_df))], ".x") %in%
      colnames(nodes_df)[1:(length(colnames(nodes_df)) - 1)]) {

    col_no_matching <-
      which(colnames(nodes_df)[1:(length(colnames(nodes_df)) - 1)] ==
              paste0(colnames(nodes_df)[length(colnames(nodes_df))], ".x"))

    # Get the column name of the new column
    new_col_name <- colnames(nodes_df)[length(colnames(nodes_df))]

    # Move contents of new column to matching column
    nodes_df[, col_no_matching] <-
      nodes_df[, length(colnames(nodes_df))]

    # Remove the last column from the ndf
    nodes_df[, length(colnames(nodes_df))] <- NULL

    # Rename the refreshed column
    colnames(nodes_df)[col_no_matching] <- new_col_name
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

  graph
}
