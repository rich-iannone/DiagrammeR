#' Set the edge attribute values to be rendered
#' @description Set a edge attribute type to display
#' as edge text when calling the \code{render_graph()}
#' function. This allows for display of different types
#' of edge attribute values on a per-edge basis.
#' Without setting the \code{display} attribute,
#' rendering a graph will default to not printing any
#' text on edges. Setting the \code{display} edge
#' attribute with this function for the first time
#' (i.e., the \code{display} column doesn't exist in
#' the graph's internal edge data frame) will insert
#' the \code{attr} value for all edges specified in
#' \code{edges} and a default value (\code{default})
#' for all remaining edges.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param attr the name of the attribute from
#' which label text for the edge will be obtained.
#' If set to \code{NULL}, then \code{NA} values
#' will be assigned to the \code{display} column
#' for the chosen edges.
#' @param edges a length vector containing one or
#' several edge ID values (as integers) for which
#' edge attributes are set for display in the
#' rendered graph. If \code{NULL}, all edges from
#' the graph are assigned the \code{display} value
#' given as \code{attr}.
#' @param default the name of an attribute to
#' set for all other graph edges not included
#' in \code{edges}. This value only gets used if
#' the \code{display} edge attribute is not in
#' the graph's internal edge data frame.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 4,
#'     m = 4,
#'     set_seed = 23) %>%
#'   set_edge_attrs(
#'     edge_attr = value,
#'     values = c(2.5, 8.2, 4.2, 2.4))
#'
#' # For edge ID values of `1`,
#' # `2`, and `3`, choose to display
#' # the edge `value` attribute (for
#' # the other edges, display nothing)
#' graph <-
#'   graph %>%
#'   set_edge_attr_to_display(
#'     edges = 1:3,
#'     attr = value,
#'     default = NA)
#'
#' # Show the graph's edge data frame; the
#' # `display` edge attribute will show, for
#' # each row, which edge attribute value to
#' # display when the graph is rendered
#' get_edge_df(graph)
#' #>  id from to  rel display value
#' #>1  1    2  1 <NA>   value   2.5
#' #>2  2    3  1 <NA>   value   8.2
#' #>3  3    3  2 <NA>   value   4.2
#' #>4  4    4  3 <NA>    <NA>   2.4
#'
#' # This function can be called multiple
#' # times on a graph; after the first time
#' # (i.e., creation of the `display`
#' # attribute), the `default` value won't
#' # be used
#' graph %>%
#'   set_edge_attr_to_display(
#'     edges = 4,
#'     attr = to) %>%
#'   set_edge_attr_to_display(
#'     edges = c(1, 3),
#'     attr = id) %>%
#'   get_edge_df()
#' #>   id from to  rel display value
#' #> 1  1    2  1 <NA>      id   2.5
#' #> 2  2    3  1 <NA>   value   8.2
#' #> 3  3    3  2 <NA>      id   4.2
#' #> 4  4    4  3 <NA>      to   2.4
#' @importFrom dplyr mutate left_join coalesce bind_cols select
#' @importFrom dplyr everything case_when tibble
#' @importFrom rlang enquo UQ
#' @export set_edge_attr_to_display

set_edge_attr_to_display <- function(graph,
                                     attr = NULL,
                                     edges = NULL,
                                     default = "label") {

  # Get the time of function start
  time_function_start <- Sys.time()

  attr <- rlang::enquo(attr)
  attr <- (rlang::UQ(attr) %>% paste())[2]

  if (attr == "NULL") {
    attr <- NULL
  }

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {

    stop(
      "The graph contains no edges, so, no edge attributes can be set.",
      call. = FALSE)
  }

  # Create bindings for specific variables
  id <- from <- to <- rel <- display <- NULL

  # Get the graph's edge data frame as an object; stop
  # function if this doesn't exist
  if (is.null(graph$edges_df)) {

    stop(
      "This graph does not contain any edges.",
      call. = FALSE)
  } else {
    edf <- graph$edges_df
  }

  # If `edges` is NULL, assume that all edges to
  # be assigned a `display` value
  if (is.null(edges)) {
    edges <- get_edge_ids(graph)
  }

  # Stop function if any of the edge ID values
  # provided in `edges` do not exist in the graph
  if (!any(edges %in% edf$id)) {

    stop(
      "One or more edge ID values in `edges` are not present in the graph.",
      call. = FALSE)
  }

  # Stop function if the edge attribute supplied as
  # `attr` does not exist in the edf
  if (!is.null(attr)) {
    if (!(attr %in% colnames(edf))) {

      stop(
        "The edge attribute given in `attr` is not in the graph's edf.",
        call. = FALSE)
    }
  }

  # If the `display` edge attribute doesn't exist,
  # create that column and fill with the default value
  if (!("display" %in% colnames(edf))) {
    edf <-
      edf %>%
      dplyr::mutate(display = as.character(default))
  }

  # Create a tibble with the edge ID values and the
  # requested edge attribute to display
  if (!is.null(attr)) {
    attr_to_display <-
      dplyr::tibble(
        id = as.integer(edges),
        display = as.character(attr))

  } else if (is.null(attr)) {
    attr_to_display <-
      dplyr::tibble(
        id = as.integer(edges),
        display = as.character("is_na"))
  }

  # Join the `attr_to_display` table with the `edf`
  edf <-
    edf %>%
    dplyr::left_join(attr_to_display, by = "id")

  # Get the column numbers for the `.x`
  # and `.y` columns
  x_col <- which(grepl("\\.x$", colnames(edf)))
  y_col <- which(grepl("\\.y$", colnames(edf)))

  # Coalesce the 2 generated columns and create a
  # single-column data frame
  if (!is.null(attr)) {
    display_col <-
      dplyr::coalesce(edf[, y_col], edf[, x_col]) %>%
      as.data.frame(stringsAsFactors = FALSE)
  } else if (is.null(attr)) {
    display_col <-
      dplyr::coalesce(edf[, y_col], edf[, x_col])

    display_col <-
      dplyr::case_when(
        display_col == "is_na" ~ as.character(NA),
        TRUE ~ display_col) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }

  # Rename the column
  colnames(display_col)[1] <- "display"

  # Remove column numbers that end with ".x" or ".y"
  edf <- edf[-which(grepl("\\.x$", colnames(edf)))]
  edf <- edf[-which(grepl("\\.y$", colnames(edf)))]

  # Bind the `display_col` df to the `edf` df and
  # modify the ordering of the columns
  edf <-
    dplyr::bind_cols(edf, display_col) %>%
    dplyr::select(
      id, from, to, rel, display, dplyr::everything())

  # Replace the graph's edge data frame with `edf`
  graph$edges_df <- edf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "set_edge_attr_to_display",
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
