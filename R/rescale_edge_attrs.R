#' Rescale numeric edge attribute values
#' @description From a graph object of class
#' \code{dgr_graph}, take a set of numeric values for
#' an edge attribute, rescale to a new numeric or color
#' range, then write to the same edge attribute or to
#' a new edge attribute column.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
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
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 7, set_seed = 3,
#'     directed = TRUE) %>%
#'   set_edge_attrs(
#'     "weight", rnorm(edge_count(.), 5))
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   from to rel           weight
#' #> 1    4  1     2.83853945848282
#' #> 2    2  4     3.67978683090796
#' #> 3    4  3     5.81040893507976
#' #> 4    3  5     6.34166517562908
#' #> 5    5  1     5.69255245195709
#' #> 6    4  5     4.67682398850357
#' #> 7    2  1     4.88269031457367
#'
#' # Rescale the `weight` edge attribute, so that
#' # its values are rescaled between 0 and 1
#' graph <-
#'   graph %>%
#'   rescale_edge_attrs("weight")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute values had been rescaled
#' get_edge_df(graph)
#' #>   from to rel weight
#' #> 1    4  1          0
#' #> 2    2  4       0.24
#' #> 3    4  3      0.848
#' #> 4    3  5          1
#' #> 5    5  1      0.815
#' #> 6    4  5      0.525
#' #> 7    2  1      0.584
#'
#' # Scale the values in the `weight` edge attribute
#' # to different shades of gray for the `color` edge
#' # attribute and different numerical values for the
#' # `penwidth` attribute
#' graph <-
#'   graph %>%
#'   rescale_edge_attrs(
#'     "weight", "gray80", "gray20", "color") %>%
#'   rescale_edge_attrs(
#'     "weight", 0.5, 3, "penwidth")
#'
#' # Get the graph's internal edf once more to show
#' # that scaled grayscale colors are now available in
#' # `color` and scaled numerical values are in the
#' # `penwidth` edge attribute
#' get_edge_df(graph)
#' #>   from to rel weight   color penwidth
#' #> 1    4  1          0 #CCCCCC      0.5
#' #> 2    2  4       0.24 #A4A4A4      1.1
#' #> 3    4  3      0.848 #484848     2.62
#' #> 4    3  5          1 #333333        3
#' #> 5    5  1      0.815 #4C4C4C    2.537
#' #> 6    4  5      0.525 #777777    1.812
#' #> 7    2  1      0.584 #6E6E6E     1.96
#' @export rescale_edge_attrs

rescale_edge_attrs <- function(graph,
                               edge_attr_from,
                               to_lower_bound = 0,
                               to_upper_bound = 1,
                               edge_attr_to = NULL,
                               from_lower_bound = NULL,
                               from_upper_bound = NULL) {

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

  return(graph)
}
