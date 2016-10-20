#' Recode a set of node attribute values
#' @description Within a graph's internal node data
#' frame (ndf), recode character or numeric node
#' attribute values. Optionally, one can specify a
#' replacement value for any unmatched mappings.
#' @param graph a graph object of class
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
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 10, set_seed = 3) %>%
#'   set_node_attrs(
#'     "shape",
#'     c("circle", "hexagon", "rectangle",
#'       "rectangle", "circle"))
#'
#' # Get the graph's internal ndf to show which
#' # node attributes are available
#' get_node_df(graph)
#' #>   id type label value     shape
#' #> 1  1          1   2.0    circle
#' #> 2  2          2   8.5   hexagon
#' #> 3  3          3   4.0 rectangle
#' #> 4  4          4   3.5 rectangle
#' #> 5  5          5   6.5    circle
#'
#' # Recode the `shape` node attribute, so that
#' # `circle` is recoded to `square` and that
#' # `rectangle` becomes `triangle`
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     "shape",
#'     "circle -> square",
#'     "rectangle -> triangle")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute values had been recoded
#' get_node_df(graph)
#' #>   id type label value    shape
#' #> 1  1          1   2.0   square
#' #> 2  2          2   8.5  hexagon
#' #> 3  3          3   4.0 triangle
#' #> 4  4          4   3.5 triangle
#' #> 5  5          5   6.5   square
#'
#' # Create a new node attribute, called `color`,
#' # that is based on a recoding of `shape`; here,
#' # map the square shape to a `red` color and map
#' # all other shapes to a `green` color
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     "shape",
#'     "square -> red",
#'     otherwise = "green",
#'     node_attr_to = "color")
#'
#' # Get the graph's internal ndf to see the change
#' get_node_df(graph)
#' #>   id type label value    shape color
#' #> 1  1          1   2.0   square   red
#' #> 2  2          2   8.5  hexagon green
#' #> 3  3          3   4.0 triangle green
#' #> 4  4          4   3.5 triangle green
#' #> 5  5          5   6.5   square   red
#'
#' # Numeric values can be recoded as well;
#' # here, perform several recodings for
#' # values of the `value` node attribute
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     "value",
#'     "2.0 -> 9.5",
#'     "4.0 -> 10.5",
#'     otherwise = 5.0)
#'
#' # Look at the graph's internal ndf
#' get_node_df(graph)
#' #>   id type label value    shape color
#' #> 1  1          1   9.5   square   red
#' #> 2  2          2   5.0  hexagon green
#' #> 3  3          3  10.5 triangle green
#' #> 4  4          4   5.0 triangle green
#' #> 5  5          5   5.0   square   red
#' @importFrom stringr str_split
#' @export recode_node_attrs

recode_node_attrs <- function(graph,
                              node_attr_from,
                              ...,
                              otherwise = NULL,
                              node_attr_to = NULL) {

  # Get list object from named vectors
  replacements <- list(...)

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's node attributes
  if (!any(column_names_graph %in% node_attr_from)) {
    stop("The node attribute to recode is not in the ndf.")
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
      stop("You cannot use those names.")
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

  return(graph)
}
