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
#' # Create 2 simple data frames to add as
#' # node attributes
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
#' # Bind the data frame as a node attribute
#' # of nodes `1` and `4`
#' graph <-
#'   graph %>%
#'   set_df_as_node_attr(
#'     node = 1,
#'     df = df_1) %>%
#'   set_df_as_node_attr(
#'     node = 4,
#'     df = df_2)
#'
#' # Get a single tibble by specifying the
#' # nodes from which there are data frames
#' # bound as node attributes
#' get_attr_dfs(
#'   graph,
#'   node_id = c(1, 4),
#'   return_format = "single_tbl")
#' #> # A tibble: 4 × 5
#' #>   node_id  type label     a     b
#' #>     <int> <chr> <chr> <chr> <dbl>
#' #> 1       1 basic     1   one     1
#' #> 2       1 basic     1   two     2
#' #> 3       4 basic     4 three     3
#' #> 4       4 basic     4  four     4
#'
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

  # Collect data frames that are attributes of graph nodes
  if (!is.null(node_id)) {

    # Get the `df_id` values for the provided nodes
    df_ids <-
      graph$nodes_df %>%
      dplyr::filter(id %in% node_id) %>%
      dplyr::select(id, type, label, dplyr::starts_with("df_id")) %>%
      tibble::as_tibble()

    # Get the appropriate rows of stored data frames
    df_storage_rows <-
      df_ids %>%
      dplyr::inner_join(graph$df_storage, by = "df_id") %>%
      dplyr::rename(node_id = id) %>%
      dplyr::select(node_id, df_data)

    # Return a data frame as a single tibble object
    if (return_format == "single_tbl") {
      for (i in 1:length(df_storage_rows$df_data)) {
        if (i == 1) {
          df <-
            df_storage_rows$df_data[[i]][[1]] %>%
            dplyr::mutate(node_id = df_storage_rows$node_id[i])
        }

        if (i > 1) {
          df <-
            bind_rows(
              df, df_storage_rows$df_data[[i]][[1]] %>%
                dplyr::mutate(node_id = df_storage_rows$node_id[i]))
        }

        if (i == length(df_storage_rows$df_data)) {
          df <-
            df %>%
            dplyr::inner_join(df_ids %>%
                                dplyr::select(id, type, label),
                              by = c("node_id" = "id")) %>%
            dplyr::select(node_id, type, label, dplyr::everything())
        }
      }
    }
  }

  return(df)
}
