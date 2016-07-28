#' Recode a set of node attribute values
#' @description Within a graph's internal node data
#' frame (ndf), recode character or numeric node
#' attribute values. Optionally, one can specify a
#' replacement value for any unmatched mappings.
#' @param graph a graph object of class
#' @param node_attr_from the name of the node attribute
#' column from which values will be recoded.
#' @param ... named vectors with values to be recoded.
#' The first component should have the value to replace
#' and the second should have the replacement value (in
#' the form \code{[to_replace] = [replacement], ...}).
#' @param otherwise an optional single value for
#' recoding any unmatched values.
#' @param node_attr_to an optional name of a new node
#' attribute to which the recoded values will be
#' applied. This will retain the original node
#' attribute and its values.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
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
#' #>   nodes type label value     shape
#' #> 1     1          1     2    circle
#' #> 2     2          2   8.5   hexagon
#' #> 3     3          3     4 rectangle
#' #> 4     4          4   3.5 rectangle
#' #> 5     5          5   6.5    circle
#'
#' # Recode the `shape` node attribute, so that
#' # `circle` is recoded to `square` and that
#' # `rectangle` becomes `triangle`
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     "shape",
#'     "circle" = "square",
#'     "rectangle" = "triangle")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute values had been recoded
#' get_node_df(graph)
#' #>   nodes type label value    shape
#' #> 1     1          1     2   square
#' #> 2     2          2   8.5  hexagon
#' #> 3     3          3     4 triangle
#' #> 4     4          4   3.5 triangle
#' #> 5     5          5   6.5   square
#'
#' # Create a new node attribute, called `color`,
#' # that is based on a recoding of `shape`; here,
#' # map the square shape to a `red` color and map
#' # all other shapes to a `green` color
#' graph <-
#'   graph %>%
#'   recode_node_attrs(
#'     "shape",
#'     "square" = "red",
#'     otherwise = "green",
#'     node_attr_to = "color")
#'
#' # Get the graph's internal ndf to see the change
#' get_node_df(graph)
#' #>   nodes type label value    shape color
#' #> 1     1          1     2   square   red
#' #> 2     2          2   8.5  hexagon green
#' #> 3     3          3     4 triangle green
#' #> 4     4          4   3.5 triangle green
#' #> 5     5          5   6.5   square   red
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

  # Extract the vector to recode from the `nodes` df
  vector_to_recode <-
    nodes[, col_num_recode]

  # Get all sets of recoding indices
  for (i in 1:length(replacements)) {
    if (i == 1) indices <- replacements
    indices[i][[1]] <-
      which(vector_to_recode %in% names(replacements[i]))
  }

  # Recode each set of indices
  for (i in 1:length(replacements)) {
    vector_to_recode[indices[i][[1]]] <-
      replacements[i][1]
  }

  # If a value is supplied for `otherwise`, apply
  # that value to all unmatched
  if (!is.null(otherwise)) {

    otherwise_indices <-
      which(!(1:nrow(nodes) %in% unlist(indices)))

    if (length(otherwise_indices) > 0) {
      vector_to_recode[otherwise_indices] <-
        otherwise
    }
  }

  # Coerce the recoded vector to a character vector
  recoded_vector <-
    as.character(vector_to_recode)

  if (!is.null(node_attr_to)){

    # Stop function if `node_attr_to` is
    # `nodes` or `node`
    if (any(c("nodes", "node") %in% node_attr_to)) {
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
        cbind(nodes, data.frame(recoded_vector))

      # Set the column name for the copied attr
      colnames(nodes)[ncol(nodes)] <- node_attr_to
    }
  } else {
    # The node attribute values will be overwritten
    # by the recoded value (no new node attrs)
    nodes[, col_num_recode] <- recoded_vector
  }

  # Create a new graph object
  dgr_graph <-
    create_graph(nodes_df = nodes,
                 edges_df = graph$edges_df,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
