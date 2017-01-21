#' Get data frames bound to node attributes
#' @description From a graph object of class
#' \code{dgr_graph}, get one or more data frames
#' already bound as node and/or edge attribute
#' values given graph node and/or edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_id a vector of node ID values in
#' which data frames are bound as node attrs.
#' @param edge_id a vector of edge ID values in
#' which data frames are bound as edge attrs.
#' @param return_format the format in which to
#' return the results of several data frames. These
#' can either be: (1) \code{single_tbl} (a tibble
#' object resulting from a `bind_rows` operation of
#' multiple data frames), (2) \code{single_df}
#' (a single data frame which all of the data frame
#' data), (3) \code{list_tbl} (a list object with a
#' tibble object present in each list component),
#' and (4) \code{list_col} (a tibble with a column
#' of tibble objects).
#' @return either a tibble, a data frame, or a list.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to")
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Create 3 simple data frames to add as
#' # attributes to nodes/edges
#' df_1 <-
#'   data.frame(
#'     a = c("one", "two"),
#'     b = c(1, 2),
#'     stringsAsFactors = FALSE)
#'
#' df_2 <-
#'   data.frame(
#'     a = c("three", "four"),
#'     b = c(3, 4),
#'     stringsAsFactors = FALSE)
#'
#' df_for_edges <-
#'   data.frame(
#'     c = c("five", "six"),
#'     d = c(5, 6),
#'     stringsAsFactors = FALSE)
#'
#' # Bind data frames as node attributes
#' # for nodes `1` and `4`; bind a data
#' # frame as an edge attribute as well
#' graph <-
#'   graph %>%
#'   set_df_as_node_attr(
#'     node = 1,
#'     df = df_1) %>%
#'   set_df_as_node_attr(
#'     node = 4,
#'     df = df_2) %>%
#'   set_df_as_edge_attr(
#'     edge = 1,
#'     df = df_for_edges)
#'
#' # Get a single tibble by specifying the
#' # nodes from which there are data frames
#' # bound as node attributes
#' get_attr_dfs(
#'   graph,
#'   node_id = c(1, 4))
#' #> # A tibble: 4 × 5
#' #>   node_id  type label     a     b
#' #>     <int> <chr> <chr> <chr> <dbl>
#' #> 1       1 basic     1   one     1
#' #> 2       1 basic     1   two     2
#' #> 3       4 basic     4 three     3
#' #> 4       4 basic     4  four     4
#'
#' # You can also get data frames that are
#' # associated with edges by using the
#' # same function
#' get_attr_dfs(
#'   graph,
#'   edge_id = 1)
#' #> # A tibble: 2 × 6
#' #>   edge_id        rel  from    to     c     d
#' #>     <int>      <chr> <int> <int> <chr> <dbl>
#' #> 1       1 leading_to     1     4  five     5
#' #> 2       1 leading_to     1     4   six     6
#'
#' # It's also possible to collect data
#' # frames associated with both nodes and
#' # edges
#' get_attr_dfs(
#'   graph,
#'   node_id = 4,
#'   edge_id = 1)
#' #> # A tibble: 4 × 11
#' #>   node_id edge_id  type label        rel  from    to
#' #>     <int>   <int> <chr> <chr>      <chr> <int> <int>
#' #> 1       4      NA basic     4       <NA>    NA    NA
#' #> 2       4      NA basic     4       <NA>    NA    NA
#' #> 3      NA       1  <NA>  <NA> leading_to     1     4
#' #> 4      NA       1  <NA>  <NA> leading_to     1     4
#' #> ... with 4 more variables: a <chr>, b <dbl>,
#' #>   c <chr>, d <dbl>
#' @importFrom dplyr filter select starts_with everything inner_join rename mutate
#' @importFrom tibble as_tibble
#' @export get_attr_dfs

get_attr_dfs <- function(graph,
                         node_id = NULL,
                         edge_id = NULL,
                         return_format = "single_tbl") {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, a df cannot be added.")
  }

  # Create bindings for specific variables
  id <- from <- to <- type <- rel <- label <- df_data <- NULL

  # Collect data frames that are attributes of graph nodes
  if (!is.null(node_id)) {

    # Get the `df_id` values for the provided nodes
    df_ids_nodes <-
      graph$nodes_df %>%
      dplyr::filter(id %in% node_id) %>%
      dplyr::select(id, type, label, dplyr::starts_with("df_id")) %>%
      tibble::as_tibble()

    # Get the appropriate rows of stored data frames
    df_storage_rows_nodes <-
      df_ids_nodes %>%
      dplyr::inner_join(graph$df_storage, by = "df_id") %>%
      dplyr::rename(node_id = id) %>%
      dplyr::select(node_id, df_data)

    # Combine multiple data frames together
    for (i in 1:length(df_storage_rows_nodes$df_data)) {
      if (i == 1) {
        df_nodes <-
          df_storage_rows_nodes$df_data[[i]][[1]] %>%
          dplyr::mutate(node_id = df_storage_rows_nodes$node_id[i])
      }

      if (i > 1) {
        df_nodes <-
          bind_rows(
            df_nodes, df_storage_rows_nodes$df_data[[i]][[1]] %>%
              dplyr::mutate(node_id = df_storage_rows_nodes$node_id[i]))
      }

      if (i == length(df_storage_rows_nodes$df_data)) {
        df_nodes <-
          df_nodes %>%
          dplyr::inner_join(df_ids_nodes %>%
                              dplyr::select(id, type, label),
                            by = c("node_id" = "id")) %>%
          dplyr::select(node_id, type, label, dplyr::everything())
      }
    }
  }

  # Collect data frames that are attributes of graph edges
  if (!is.null(edge_id)) {

    # Get the `df_id` values for the provided edges
    df_ids_edges <-
      graph$edges_df %>%
      dplyr::filter(id %in% edge_id) %>%
      dplyr::select(id, from, to, rel, dplyr::starts_with("df_id")) %>%
      tibble::as_tibble()

    # Get the appropriate rows of stored data frames
    df_storage_rows_edges <-
      df_ids_edges %>%
      dplyr::inner_join(graph$df_storage, by = "df_id") %>%
      dplyr::rename(edge_id = id) %>%
      dplyr::select(edge_id, df_data)

    # Combine multiple data frames together
    for (i in 1:length(df_storage_rows_edges$df_data)) {
      if (i == 1) {
        df_edges <-
          df_storage_rows_edges$df_data[[i]][[1]] %>%
          dplyr::mutate(edge_id = df_storage_rows_edges$edge_id[i])
      }

      if (i > 1) {
        df_edges <-
          bind_rows(
            df_edges, df_storage_rows_edges$df_data[[i]][[1]] %>%
              dplyr::mutate(edge_id = df_storage_rows_edges$edge_id[i]))
      }

      if (i == length(df_storage_rows_edges$df_data)) {
        df_edges <-
          df_edges %>%
          dplyr::inner_join(df_ids_edges %>%
                              dplyr::select(id, rel, from, to),
                            by = c("edge_id" = "id")) %>%
          dplyr::select(edge_id, rel, from, to, dplyr::everything())
      }
    }
  }

  if (exists("df_nodes") & exists("df_edges")) {
    df <-
      dplyr::bind_rows(df_nodes, df_edges) %>%
      dplyr::select(node_id, edge_id, type, label,
             rel, from, to, dplyr::everything())
  }

  if (exists("df_nodes") & !exists("df_edges")) {
    df <- df_nodes %>%
      dplyr::select(node_id, type, label, dplyr::everything())
  }

  if (!exists("df_nodes") & exists("df_edges")) {
    df <- df_edges %>%
      dplyr::select(edge_id, rel, from, to, dplyr::everything())
  }

  if (!exists("df_nodes") & !exists("df_edges")) {
    df <- NA
  }

  return(df)
}
