#' Mutate a set of edge attribute values
#' @description Within a graph's internal edge data
#' frame (edf), mutate numeric edge attribute values
#' using one or more expressions. Optionally, one can
#' specify a different edge attribute name and create
#' a new edge attribute while retaining the original
#' edge attribute and its values.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr_from the name of the edge attribute
#' column from which values will be mutated. An
#' \code{NA} value can be provided here if edge
#' attribute names are used in \code{expressions}
#' statements. Note that if \code{NA} is used, a value
#' must be provided for \code{edge_attr_to}.
#' @param expressions one or more expressions for
#' mutation of all edge attribute values specified by
#' \code{edge_attr_from}. To reference the edge
#' attribute given in \code{edge_attr_from}, use the
#' \code{~} character. Otherwise, all edge attributes
#' can be referenced by using the names of those edge
#' attributes directly in the expressions. Expressions
#' are evaluated in the order provided.
#' @param edge_attr_to an optionally supplied name of
#' a new edge attribute to which the mutated values
#' will be applied. This will retain the original edge
#' attribute and its values. If \code{NA} is used
#' with \code{edge_attr_from}, a value must be provided
#' here (since mutated values must be placed
#' somewhere).
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 3 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 4) %>%
#'   set_edge_attrs(
#'     edge_attr = "width",
#'     values = c(3.4, 2.3, 7.2))
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   id from to  rel width
#' #> 1  1    1  2 <NA>   3.4
#' #> 2  2    2  3 <NA>   2.3
#' #> 3  3    3  4 <NA>   7.2
#'
#' # Mutate the `width` edge attribute, dividing
#' # each value by 2
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     edge_attr_from = "width",
#'     expressions = "~ / 2")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute `width` had its values changed
#' get_edge_df(graph)
#' #>   id from to  rel width
#' #> 1  1    1  2 <NA>  1.70
#' #> 2  2    2  3 <NA>  1.15
#' #> 3  3    3  4 <NA>  3.60
#'
#' # Create a new edge attribute, called `length`,
#' # that is the log of values in `width` plus 2
#' # (and round all values to 2 decimal places)
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     edge_attr_from = "width",
#'     expressions = "round(log(~) + 2, 2)",
#'     edge_attr_to = "length")
#'
#' # Get the graph's internal edf to show that
#' # the edge attribute values had been mutated
#' # and used as the new edge attribute `length`
#' get_edge_df(graph)
#' #>   id from to  rel width length
#' #> 1  1    1  2 <NA>  1.70   2.53
#' #> 2  2    2  3 <NA>  1.15   2.14
#' #> 3  3    3  4 <NA>  3.60   3.28
#'
#' # Create a new edge attribute called `area`,
#' # which is the product of the `width` and
#' # `length` attributes; note that we can provide
#' # NA to `edge_attr_from` since we are naming
#' # at least one of the edge attributes in the
#' # `expressions` vector (and providing a new
#' # edge attribute name: `area`)
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     edge_attr_from = NA,
#'     expressions = "width * length",
#'     edge_attr_to = "area")
#'
#' # Get the graph's internal edf to show that
#' # the edge attribute values had been multiplied
#' # together, creating a new edge attribute `area`
#' get_edge_df(graph)
#' #>   id from to  rel width length   area
#' #> 1  1    1  2 <NA>  1.70   2.53  4.301
#' #> 2  2    2  3 <NA>  1.15   2.14  2.461
#' #> 3  3    3  4 <NA>  3.60   3.28 11.808
#' @importFrom stringr str_replace_all str_detect
#' @importFrom dplyr mutate_
#' @export mutate_edge_attrs

mutate_edge_attrs <- function(graph,
                              edge_attr_from,
                              expressions,
                              edge_attr_to = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Extract the graph's edf
  edf <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edf)

  # Stop function if `edge_attr_from` is not one
  # of the graph's edge attributes
  if (!is.na(edge_attr_from)) {
    if (!any(column_names_graph %in% edge_attr_from)) {
      stop("The edge attribute to mutate is not in the edf.")
    }
  }

  # Replace `~` with edge attribute undergoing mutation
  if (!is.na(edge_attr_from)) {
    expressions <-
      stringr::str_replace_all(
        expressions, "~", edge_attr_from)
  }

  # Stop function if NA provided for `edge_attr_from` but
  # no value provided for `edge_attr_to`
  if (is.na(edge_attr_from)) {
    if (is.null(edge_attr_to)) {
      stop("If NA provided for `edge_attr_from`, a value must be provided for `edge_attr_to`.")
    }
  }

  # If NA provided for `edge_attr_from`, ensure that at
  # least one edge attribute value exists in each of the
  # provided expressions
  if (is.na(edge_attr_from)) {
    for (i in 1:length(expressions)) {
      if (all(
        stringr::str_detect(
          expressions[i],
          column_names_graph) == FALSE)) {
        stop("At least one edge attribute should exist in `expressions`.")
      }
    }
  }

  if (!is.null(edge_attr_to)) {

    # Stop function if `edge_attr_to` is `from` or `to`
    if (any(c("from", "to") %in% edge_attr_to)) {
      stop("You cannot either `from` or `to` as `edge_attr_to` values.")
    }

    for (i in 1:length(expressions)) {
      edf <-
        edf %>%
        dplyr::mutate_(.dots = setNames(list(expressions[i]), edge_attr_to))
    }
  } else {
    for (i in 1:length(expressions)) {
      edf <-
        edf %>%
        dplyr::mutate_(.dots = setNames(list(expressions[i]), edge_attr_from))
    }
  }

  # Update the graph
  graph$edges_df <- edf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "mutate_edge_attrs",
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
