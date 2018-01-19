#' Set a data frame as a node attribute
#' @description From a graph object of class
#' \code{dgr_graph}, bind a data frame as a node
#' attribute property for one given graph node. The
#' data frames are stored in list columns within
#' a \code{df_tbl} object. A \code{df_id} value is
#' generated and serves as a pointer to the table
#' row that contains the ingested data frame.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node the node ID to which the data frame
#' will be bound as an attribute.
#' @param df the data frame to be bound to the
#' node as an attribute.
#' @return a graph object of class \code{dgr_graph}.
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
#' # Create a simple data frame to add as
#' # a node attribute
#' df <-
#'   data.frame(
#'     a = c("one", "two", "three"),
#'     b = c(1, 2, 3),
#'     stringsAsFactors = FALSE)
#'
#' # Bind the data frame as a node attribute
#' # of node `1`
#' graph <-
#'   set_df_as_node_attr(
#'     graph = graph,
#'     node = 1,
#'     df = df)
#'
#' # Create another data frame to add as
#' # a node attribute
#' df_2 <-
#'   data.frame(
#'     c = c("four", "five", "six"),
#'     d = c(4, 5, 6),
#'     stringsAsFactors = FALSE)
#'
#' # Bind the data frame as a node attribute
#' # of node `2`
#' graph <-
#'   graph %>%
#'   set_df_as_node_attr(
#'     node = 2,
#'     df = df_2)
#' @importFrom dplyr filter everything mutate select bind_rows as_tibble
#' @importFrom purrr flatten_chr
#' @export set_df_as_node_attr

set_df_as_node_attr <- function(graph,
                                node,
                                df) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, a df cannot be added.",
      call. = FALSE)
  }

  # Value given for node must only be a single value
  if (length(node) > 1) {

    stop(
      "Only one node can be specified.",
      call. = FALSE)
  }

  # Value given for node must correspond to a node ID
  # in the graph
  if (!(node %in% graph$nodes_df$id)) {

    stop(
      "The value given for `node` does not correspond to a node ID.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  df_id__ <- node_edge__ <- id__ <- NULL

  # Generate an empty `df_storage` list if not present
  # TODO: put this in `create_graph()`
  if (is.null(graph$df_storage)) {
    graph$df_storage <- list()
  }

  # Generate a random 8-character, alphanumeric
  # string to use as a data frame ID (`df_id`)
  df_id <-
    replicate(
      8, sample(c(LETTERS, letters, 0:9), 1)) %>%
    paste(collapse = "")

  # Mutate the incoming data frame to contain
  # identifying information
  df <-
    df %>%
    dplyr::mutate(
      df_id__ = df_id,
      node_edge__ = "node",
      id__ = node) %>%
    dplyr::select(df_id__, node_edge__, id__, everything()) %>%
    dplyr::as_tibble()

  # If there is an existing data frame attributed
  # to the node, remove it
  if (dplyr::bind_rows(graph$df_storage) %>%
      dplyr::filter(node_edge__ == "node") %>%
      dplyr::filter(id__ == node) %>%
      nrow() > 0) {
    df_object_old <-
      (dplyr::bind_rows(graph$df_storage) %>%
         dplyr::filter(node_edge__ == "node") %>%
         dplyr::filter(id__ == node) %>%
         dplyr::select(df_id__) %>%
         purrr::flatten_chr())[1]

    graph$df_storage[[`df_object_old`]] <- NULL
  }

  # Bind the data frame to `df_storage` list component
  graph$df_storage[[`df_id`]] <- df

  # Set the `df_id` node attribute using the
  # `set_node_attrs()` function
  graph <-
    set_node_attrs(
      graph = graph,
      node_attr = "df_id",
      values = df_id,
      nodes = node)

  # Update the `graph_log` df with an action
  graph$graph_log <-
    graph$graph_log[-nrow(graph$graph_log),] %>%
    add_action_to_log(
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_df_as_node_attr",
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
