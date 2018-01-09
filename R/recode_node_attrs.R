#' Recode a set of node attribute values
#' @description Within a graph's internal node data
#' frame (ndf), recode character or numeric node
#' attribute values. Optionally, one can specify a
#' replacement value for any unmatched mappings.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr_from the name of the node attribute
#' column from which values will be recoded.
#' @param ... single-length character vectors with
#' the recoding instructions. The first component should
#' have the value to replace and the second should have
#' the replacement value (in the form
#' \code{"[to_replace] -> [replacement]", ...}).
#' @param otherwise an optional single value for
#' recoding any unmatched values.
#' @param node_attr_to an optional name of a new node
#' attribute to which the recoded values will be
#' applied. This will retain the original node
#' attribute and its values.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 5,
#'     m = 10,
#'     set_seed = 23) %>%
#'   set_node_attrs(
#'     node_attr = shape,
#'     values =
#'       c("circle", "hexagon",
#'         "rectangle", "rectangle",
#'         "circle"))
#'
#' # Get the graph's internal ndf
#' # to show which node
#' # attributes are available
#' get_node_df(graph)
#' #>   id type label     shape
#' #> 1  1 <NA>  <NA>    circle
#' #> 2  2 <NA>  <NA>   hexagon
#' #> 3  3 <NA>  <NA> rectangle
#' #> 4  4 <NA>  <NA> rectangle
#' #> 5  5 <NA>  <NA>    circle
#'
#' # Recode the `shape` node
#' # attribute, so that `circle`
#' # is recoded to `square` and that
#' # `rectangle` becomes `triangle`
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     node_attr_from = shape,
#'     "circle -> square",
#'     "rectangle -> triangle")
#'
#' # Get the graph's internal
#' # ndf to show that the node
#' # attribute values had been recoded
#' get_node_df(graph)
#' #>   id type label    shape
#' #> 1  1 <NA>  <NA>   square
#' #> 2  2 <NA>  <NA>  hexagon
#' #> 3  3 <NA>  <NA> triangle
#' #> 4  4 <NA>  <NA> triangle
#' #> 5  5 <NA>  <NA>   square
#'
#' # Create a new node attribute,
#' # called `color`, that is based
#' # on a recoding of `shape`; here,
#' # map the square shape to a `red`
#' # color and map all other shapes
#' # to a `green` color
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     node_attr_from = shape,
#'     "square -> red",
#'     otherwise = "green",
#'     node_attr_to = color)
#'
#' # Get the graph's internal ndf
#' # to see the change
#' get_node_df(graph)
#' #>   id type label    shape color
#' #> 1  1 <NA>  <NA>   square   red
#' #> 2  2 <NA>  <NA>  hexagon green
#' #> 3  3 <NA>  <NA> triangle green
#' #> 4  4 <NA>  <NA> triangle green
#' #> 5  5 <NA>  <NA>   square   red
#' @importFrom stringr str_split
#' @importFrom rlang enquo UQ
#' @export recode_node_attrs

recode_node_attrs <- function(graph,
                              node_attr_from,
                              ...,
                              otherwise = NULL,
                              node_attr_to = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  node_attr_from <- rlang::enquo(node_attr_from)
  node_attr_from <- (rlang::UQ(node_attr_from) %>% paste())[2]

  node_attr_to <- rlang::enquo(node_attr_to)
  node_attr_to <- (rlang::UQ(node_attr_to) %>% paste())[2]

  if (node_attr_to == "NULL") {
    node_attr_to <- NULL
  }

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {

    stop(
      "The graph contains no nodes, so, no nodes can be recoded.",
      call. = FALSE)
  }

  # Get list object from named vectors
  replacements <- list(...)

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's node attributes
  if (!any(column_names_graph %in% node_attr_from)) {

    stop(
      "The node attribute to recode is not in the ndf.",
      call. = FALSE)
  }

  # Get the column number for the node attr to recode
  col_num_recode <-
    which(colnames(nodes) %in% node_attr_from)

  # Get the column class for the node attr to recode
  node_attr_class <- class(nodes[, col_num_recode])

  # Extract the vector to recode from the `nodes` df
  vector_to_recode <- nodes[, col_num_recode]

  # Initialize the `indices_stack` vector
  indices_stack <- vector("numeric")

  # Parse the recoding pairs
  for (i in 1:length(replacements)) {

    pairing <-
      trimws(unlist(stringr::str_split(replacements[[i]], "->")))

    if (node_attr_class == "numeric") {
      pairing <- as.numeric(pairing)
    }

    indices <- which(vector_to_recode %in% pairing[1])

    vector_to_recode[setdiff(indices, indices_stack)] <- pairing[2]

    indices_stack <- c(indices_stack, indices)
  }

  # If a value is supplied for `otherwise`, apply
  # that value to all unmatched
  if (!is.null(otherwise)) {

    otherwise_indices <-
      which(!(1:nrow(nodes) %in% indices_stack))

    if (length(otherwise_indices) > 0) {
      vector_to_recode[otherwise_indices] <-
        otherwise
    }
  }

  # Rename the `vector_to_recode` object
  recoded_vector <- vector_to_recode

  if (!is.null(node_attr_to)) {

    # Stop function if `node_attr_to` is
    # `id` or `nodes`
    if (any(c("id", "nodes") %in% node_attr_to)) {

      stop(
        "You cannot use those names.",
        call. = FALSE)
    }

    if (any(column_names_graph %in% node_attr_to)) {

      # Node attribute exists and values will be
      # overwritten in this case
      col_num_write_to <-
        which(column_names_graph %in% node_attr_to)

      nodes[, col_num_write_to] <-
        recoded_vector
    }

    if (!any(column_names_graph %in% node_attr_to)) {

      # Node attribute does not exist and values be
      # part of a new node attribute
      nodes <-
        cbind(nodes,
              data.frame(recoded_vector,
                         stringsAsFactors = FALSE))

      # Set the column name for the copied attr
      colnames(nodes)[ncol(nodes)] <- node_attr_to
    }
  } else {
    # The node attribute values will be overwritten
    # by the recoded value (no new node attrs)
    nodes[, col_num_recode] <- recoded_vector
  }

  # Replace the `nodes_df` object in the graph
  # with the `nodes` object
  graph$nodes_df <- nodes

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "recode_node_attrs",
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
