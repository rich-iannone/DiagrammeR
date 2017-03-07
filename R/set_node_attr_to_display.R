#' Set the node attribute values to be rendered
#' @description Set a node attribute type to display
#' as node text when calling the \code{render_graph()}
#' function. This allows for display of different types
#' of node attribute values on a per-node basis.
#' Without setting the \code{display} attribute,
#' rendering a graph will default to printing text
#' from the \code{label} attribute on nodes. Setting
#' the \code{display} node attribute with this function
#' for the first time (i.e., the \code{display} column
#' doesn't exist in the graph's internal node data frame)
#' will insert the \code{attr} value for all nodes
#' specified in \code{nodes} and a default value
#' (\code{default}) for all remaining nodes.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param attr the name of the attribute to
#' set for the \code{type} of global attribute
#' specified. If set to \code{NULL}, then \code{NA}
#' values will be assigned to the \code{display}
#' for the chosen nodes.
#' @param nodes a length vector containing one or
#' several node ID values (as integers) for which
#' node attributes are set for display in the
#' rendered graph. If \code{NULL}, all nodes from
#' the graph are assigned the \code{display} value
#' given as \code{attr}.
#' @param default the name of an attribute to
#' set for all other graph nodes not included
#' in \code{nodes}. This value only gets used if
#' the \code{display} node attribute is not in
#' the graph's internal node data frame.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 5, set_seed = 23)
#'
#' # For node ID values of `1` to `3`, choose
#' # to display the node `value` attribute (for
#' # the other nodes, display nothing)
#' graph <-
#'   graph %>%
#'   set_node_attr_to_display(
#'     nodes = 1:3, attr = "value", default = NA)
#'
#' # Show the graph's node data frame; the
#' # `display` node attribute will show for
#' # each row, which node attribute value to
#' # display when the graph is rendered
#' get_node_df(graph)
#' #>   id type label display value
#' #> 1  1 <NA>     1   value   6.0
#' #> 2  2 <NA>     2   value   2.5
#' #> 3  3 <NA>     3   value   3.5
#' #> 4  4 <NA>     4    <NA>   7.5
#' #> 5  5 <NA>     5    <NA>   8.5
#'
#' # This function can be called multiple
#' # times on a graph; after the first time
#' # (i.e., creation of the `display`
#' # attribute), the `default` value won't
#' # be used
#' graph %>%
#'   set_node_attr_to_display(
#'     nodes = 4, attr = "label") %>%
#'   set_node_attr_to_display(
#'     nodes = c(1, 5), attr = "id") %>%
#'   get_node_df()
#' #>   id type label display value
#' #> 1  1 <NA>     1      id   6.0
#' #> 2  2 <NA>     2   value   2.5
#' #> 3  3 <NA>     3   value   3.5
#' #> 4  4 <NA>     4   label   7.5
#' #> 5  5 <NA>     5      id   8.5
#' @importFrom dplyr mutate left_join coalesce bind_cols select everything case_when
#' @importFrom tibble tibble
#' @export set_node_attr_to_display

set_node_attr_to_display <- function(graph,
                                     attr = NULL,
                                     nodes = NULL,
                                     default = "label") {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no selections can be made.")
  }

  # Create bindings for specific variables
  id <- type <- label <- display <- type <- NULL

  # Get the graph's node data frame as an object; stop
  # function if this doesn't exist
  if (is.null(graph$nodes_df)) {
    stop("This graph does not contain any nodes.")
  } else {
    ndf <- graph$nodes_df
  }

  # If `nodes` is NULL, assume that all nodes to
  # be assigned a `display` value
  if (is.null(nodes)) {
    nodes <- get_node_ids(graph)
  }

  # Stop function if any of the node ID values
  # provided in `nodes` do not exist in the graph
  if (!any(nodes %in% ndf$id)) {
    stop("One or more node ID values in `nodes` are not present in the graph.")
  }

  # Stop function if the node attribute supplied as
  # `attr` does not exist in the ndf
  if (!is.null(attr)) {
    if (!(attr %in% colnames(ndf))) {
      stop("The node attribute given in `attr` is not in the graph's ndf.")
    }
  }

  # If the `display` node attribute doesn't exist,
  # create that column and fill with the default value
  if (!("display" %in% colnames(ndf))) {
    ndf <-
      ndf %>%
      dplyr::mutate(display = as.character(default))
  }

  # Create a tibble with the node ID values and the
  # requested node attribute to display
  if (!is.null(attr)) {
    attr_to_display <-
      tibble::tibble(
        id = as.integer(nodes),
        display = as.character(attr))

  } else if (is.null(attr)) {
    attr_to_display <-
      tibble::tibble(
        id = as.integer(nodes),
        display = as.character("is_na"))
  }

  # Join the `attr_to_display` table with the `ndf`
  ndf <-
    ndf %>%
    dplyr::left_join(attr_to_display, by = "id")

  # Get the column numbers for the `.x`
  # and `.y` columns
  x_col <- which(grepl("\\.x$", colnames(ndf)))
  y_col <- which(grepl("\\.y$", colnames(ndf)))

  # Coalesce the 2 generated columns and create a
  # single-column data frame
  if (!is.null(attr)) {
    display_col <-
      dplyr::coalesce(ndf[, y_col], ndf[, x_col]) %>%
      as.data.frame(stringsAsFactors = FALSE)
  } else if (is.null(attr)) {
    display_col <-
      dplyr::coalesce(ndf[, y_col], ndf[, x_col])

    display_col <-
      dplyr::case_when(
        display_col == "is_na" ~ as.character(NA),
        TRUE ~ display_col) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }

  # Rename the column
  colnames(display_col)[1] <- "display"

  # Remove column numbers that end with ".x" or ".y"
  ndf <- ndf[-which(grepl("\\.x$", colnames(ndf)))]
  ndf <- ndf[-which(grepl("\\.y$", colnames(ndf)))]

  # Bind the `display_col` df to the `ndf` df and
  # modify the ordering of the columns
  ndf <-
    dplyr::bind_cols(ndf, display_col) %>%
    dplyr::select(
      id, type, label, display, dplyr::everything())

  # Replace the graph's node data frame with `ndf`
  graph$nodes_df <- ndf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_node_attr_to_display",
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
