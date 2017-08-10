#' Mutate a set of node attribute values
#' @description Within a graph's internal node data
#' frame (ndf), mutate numeric node attribute values
#' using one or more expressions. Optionally, one can
#' specify a different node attribute name and create
#' a new node attribute while retaining the original
#' node attribute and its values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param node_attr_from the name of the node attribute
#' column from which values will be mutated. An
#' \code{NA} value can be provided here if node
#' attribute names are used in \code{expressions}
#' statements. Note that if \code{NA} is used, a value
#' must be provided for \code{node_attr_to}.
#' @param expressions one or more expressions for
#' mutation of all node attribute values specified by
#' \code{node_attr_from}. To reference the node
#' attribute given in \code{node_attr_from}, use the
#' \code{~} character. Otherwise, all node attributes
#' can be referenced by using the names of those node
#' attributes directly in the expressions. Expressions
#' are evaluated in the order provided.
#' @param node_attr_to an optionally supplied name of
#' a new node attribute to which the mutated values
#' will be applied. This will retain the original node
#' attribute(s) and its values. If \code{NA} is used
#' with \code{node_attr_from}, a value must be provided
#' here (since mutated values must be placed
#' somewhere).
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 3 nodes
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 3) %>%
#'   set_node_attrs(
#'     node_attr = width,
#'     values = c(3.4, 2.3, 7.2))
#'
#' # Get the graph's internal ndf to show which
#' # node attributes are available
#' get_node_df(graph)
#' #>   id type label width
#' #> 1  1 <NA>     1   3.4
#' #> 2  2 <NA>     2   2.3
#' #> 3  3 <NA>     3   7.2
#'
#' # Mutate the `width` node attribute, dividing
#' # each value by 2
#' graph <-
#'   graph %>%
#'   mutate_node_attrs(
#'     node_attr_from = width,
#'     expressions = "~ / 2")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute `width` had its values changed
#' get_node_df(graph)
#' #>   id type label width
#' #> 1  1 <NA>     1  1.70
#' #> 2  2 <NA>     2  1.15
#' #> 3  3 <NA>     3  3.60
#'
#' # Create a new node attribute, called `length`,
#' # that is the log of values in `width` plus 2
#' # (and round all values to 2 decimal places)
#' graph <-
#'   graph %>%
#'   mutate_node_attrs(
#'     node_attr_from = width,
#'     expressions = "round(log(~) + 2, 2)",
#'     node_attr_to = length)
#'
#' # Get the graph's internal ndf to show that
#' # the node attribute values had been mutated
#' # and used as the new node attribute `length`
#' get_node_df(graph)
#' #>   id type label width length
#' #> 1  1 <NA>     1  1.70   2.53
#' #> 2  2 <NA>     2  1.15   2.14
#' #> 3  3 <NA>     3  3.60   3.28
#'
#' # Create a new node attribute called `area`,
#' # which is the product of the `width` and
#' # `length` attributes; note that we can provide
#' # NA to `node_attr_from` since we are naming
#' # at least one of the node attributes in the
#' # `expressions` vector (and providing a new
#' # node attribute name: `area`)
#' graph <-
#'   graph %>%
#'   mutate_node_attrs(
#'     node_attr_from = NA,
#'     expressions = "width * length",
#'     node_attr_to = area)
#'
#' # Get the graph's internal ndf to show that
#' # the node attribute values had been multiplied
#' # together, creating a new node attribute `area`
#' get_node_df(graph)
#' #>   id type label width length   area
#' #> 1  1 <NA>     1  1.70   2.53  4.301
#' #> 2  2 <NA>     2  1.15   2.14  2.461
#' #> 3  3 <NA>     3  3.60   3.28 11.808
#' @importFrom stringr str_replace_all str_detect
#' @importFrom dplyr mutate_
#' @importFrom rlang enquo UQ
#' @export mutate_node_attrs

mutate_node_attrs <- function(graph,
                              node_attr_from,
                              expressions,
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

  if (node_attr_from == "NA") {
    node_attr_from <- as.character(NA)
  }

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Extract the graph's ndf
  ndf <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(ndf)

  # Stop function if `node_attr_from` is not one
  # of the graph's node attributes
  if (!is.na(node_attr_from)) {
    if (!any(column_names_graph %in% node_attr_from)) {
      stop("The node attribute to mutate is not in the ndf.")
    }
  }

  # Replace `~` with node attribute undergoing mutation
  if (!is.na(node_attr_from)) {
    expressions <-
      stringr::str_replace_all(
        expressions, "~", node_attr_from)
  }

  # Stop function if NA provided for `node_attr_from` but
  # no value provided for `node_attr_to`
  if (is.na(node_attr_from)) {
    if (is.null(node_attr_to)) {
      stop("If NA provided for `node_attr_from`, a value must be provided for `node_attr_to`.")
    }
  }

  # If NA provided for `node_attr_from`, ensure that at
  # least one node attribute value exists in each of the
  # provided expressions
  if (is.na(node_attr_from)) {
    for (i in 1:length(expressions)) {
      if (all(
        stringr::str_detect(
          expressions[i],
          column_names_graph[-1]) == FALSE)) {
        stop("At least one node attribute should exist in `expressions`.")
      }
    }
  }

  if (!is.null(node_attr_to)) {

    # Stop function if `node_attr_to` is `id`
    if (node_attr_to == "id") {
      stop("You cannot `id` as the value for `node_attr_to`.")
    }

    for (i in 1:length(expressions)) {
      ndf <-
        ndf %>%
        dplyr::mutate_(.dots = setNames(list(expressions[i]), node_attr_to))
    }
  } else {
    for (i in 1:length(expressions)) {
      ndf <-
        ndf %>%
        dplyr::mutate_(.dots = setNames(list(expressions[i]), node_attr_from))
    }
  }

  # Update the graph
  graph$nodes_df <- ndf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "mutate_node_attrs",
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
