#' Rescale numeric edge attribute values
#' @description From a graph object of class
#' \code{dgr_graph}, take a set of numeric values for
#' an edge attribute, rescale to a new numeric or color
#' range, then write to the same edge attribute or to
#' a new edge attribute column.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param edge_attr_from the edge attribute containing
#' numeric data that is to be rescaled to new numeric
#' or color values.
#' @param to_lower_bound the lower bound value for the
#' set of rescaled values. This can be a numeric value
#' or an X11 color name.
#' @param to_upper_bound the upper bound value for the
#' set of rescaled values. This can be a numeric value
#' or an X11 color name.
#' @param edge_attr_to an optional name of a new edge
#' attribute to which the recoded values will be
#' applied. This will retain the original edge
#' attribute and its values.
#' @param from_lower_bound an optional, manually set
#' lower bound value for the rescaled values. If not
#' set, the minimum value from the set will be used.
#' @param from_upper_bound an optional, manually set
#' upper bound value for the rescaled values. If not
#' set, the minimum value from the set will be used.
#' @return a graph object of class \code{dgr_graph}.
#' @import scales
#' @importFrom grDevices colors
#' @examples
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     n = 5, m = 7,
#'     set_seed = 23,
#'     directed = TRUE) %>%
#'   set_edge_attrs(
#'     edge_attr = "weight",
#'     values = rnorm(edge_count(.), 5))
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   id from to  rel   weight
#' #> 1  1    2  3 <NA> 5.218288
#' #> 2  2    3  5 <NA> 3.953465
#' #> 3  3    3  4 <NA> 4.711311
#' #> 4  4    2  4 <NA> 5.481550
#' #> 5  5    2  5 <NA> 3.783624
#' #> 6  6    4  5 <NA> 5.308137
#' #> 7  7    1  4 <NA> 4.479822
#'
#' # Rescale the `weight` edge attribute, so that
#' # its values are rescaled between 0 and 1
#' graph <-
#'   graph %>%
#'   rescale_edge_attrs(
#'     edge_attr_from = "weight")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute values had been rescaled
#' get_edge_df(graph)
#' #>   id from to  rel weight
#' #> 1  1    2  3 <NA>  0.845
#' #> 2  2    3  5 <NA>  0.100
#' #> 3  3    3  4 <NA>  0.546
#' #> 4  4    2  4 <NA>  1.000
#' #> 5  5    2  5 <NA>  0.000
#' #> 6  6    4  5 <NA>  0.898
#' #> 7  7    1  4 <NA>  0.410
#'
#' # Scale the values in the `weight` edge attribute
#' # to different shades of gray for the `color` edge
#' # attribute and different numerical values for the
#' # `penwidth` attribute
#' graph <-
#'   graph %>%
#'   rescale_edge_attrs(
#'     edge_attr_from = "weight",
#'     to_lower_bound = "gray80",
#'     to_upper_bound = "gray20",
#'     edge_attr_to = "color") %>%
#'   rescale_edge_attrs(
#'     edge_attr_from = "weight",
#'     to_lower_bound = 0.5,
#'     to_upper_bound = 3,
#'     edge_attr_to = "penwidth")
#'
#' # Get the graph's internal edf once more to show
#' # that scaled grayscale colors are now available in
#' # `color` and scaled numerical values are in the
#' # `penwidth` edge attribute
#' get_edge_df(graph)
#' #>   id from to  rel weight   color penwidth
#' #> 1  1    2  3 <NA>  0.845 #484848    2.612
#' #> 2  2    3  5 <NA>  0.100 #BBBBBB    0.750
#' #> 3  3    3  4 <NA>  0.546 #747474    1.865
#' #> 4  4    2  4 <NA>  1.000 #333333    3.000
#' #> 5  5    2  5 <NA>  0.000 #CCCCCC    0.500
#' #> 6  6    4  5 <NA>  0.898 #414141    2.745
#' #> 7  7    1  4 <NA>  0.410 #898989    1.525
#' @export rescale_edge_attrs

rescale_edge_attrs <- function(graph,
                               edge_attr_from,
                               to_lower_bound = 0,
                               to_upper_bound = 1,
                               edge_attr_to = NULL,
                               from_lower_bound = NULL,
                               from_upper_bound = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no edge attributes can be rescaled.")
  }

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's edge attributes
  if (!any(column_names_graph %in% edge_attr_from)) {
    stop("The edge attribute to rescale is not in the edf.")
  }

  # Get the column number for the edge attr to rescale
  col_num_rescale <-
    which(colnames(edges) %in% edge_attr_from)

  # Extract the vector to rescale from the `edges` df
  vector_to_rescale <- as.numeric(edges[, col_num_rescale])

  if ((!is.null(from_lower_bound) &
       is.null(from_upper_bound)) |
      (is.null(from_lower_bound) &
       !is.null(from_upper_bound)) |
      (is.null(from_lower_bound) &
       is.null(from_upper_bound))) {

    from <- range(vector_to_rescale,
                  na.rm = TRUE, finite = TRUE)
  } else {
    from <- c(from_lower_bound, from_upper_bound)
  }

  # Get vector of rescaled, numeric edge
  # attribute values
  if (is.numeric(to_lower_bound) &
      is.numeric(to_upper_bound)) {

    edges_attr_vector_rescaled <-
      round(
        rescale(
          x = vector_to_rescale,
          to = c(to_lower_bound,
                 to_upper_bound),
          from = from),
        3)
  }

  # Get vector of rescaled, edge attribute color values
  if ((to_lower_bound %in% colors()) &
      (to_upper_bound %in% colors())) {

    edges_attr_vector_rescaled <-
      cscale(
        x = vector_to_rescale,
        palette = seq_gradient_pal(to_lower_bound,
                                   to_upper_bound))
  }

  # If a new edge attribute name was not provided,
  # overwrite the source edge attribute with the
  # rescaled values
  if (is.null(edge_attr_to)) {
    edge_attr_to <- edge_attr_from
  }

  # Set the edge attribute values for edges specified
  # in selection
  graph <-
    set_edge_attrs(
      x = graph,
      edge_attr = edge_attr_to,
      values = edges_attr_vector_rescaled)

  # Remove last action from the `graph_log`
  graph$graph_log <- graph$graph_log[1:(nrow(graph$graph_log) - 1), ]

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "rescale_edge_attrs",
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
