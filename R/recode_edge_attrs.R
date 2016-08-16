#' Recode a set of edge attribute values
#' @description Within a graph's internal edge data
#' frame (edf), recode character or numeric edge
#' attribute values. Optionally, one can specify a
#' replacement value for any unmatched mappings.
#' @param graph a graph object of class
#' @param edge_attr_from the name of the edge attribute
#' column from which values will be recoded.
#' @param ... named vectors with values to be recoded.
#' The first component should have the value to replace
#' and the second should have the replacement value (in
#' the form \code{[to_replace] = [replacement], ...}).
#' @param otherwise an optional single value for
#' recoding any unmatched values.
#' @param edge_attr_to an optional name of a new edge
#' attribute to which the recoded values will be
#' applied. This will retain the original edge
#' attribute and its values.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     4, 6, set_seed = 3) %>%
#'   set_edge_attrs(
#'     "rel",
#'     c("a", "b", "a",
#'       "c", "b", "d"))
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   from to rel
#' #> 1    3  2   a
#' #> 2    1  2   b
#' #> 3    4  3   a
#' #> 4    1  4   c
#' #> 5    1  3   b
#' #> 6    2  4   d
#'
#' # Recode the `rel` node attribute, creating a
#' # new edge attribute called `penwidth`; here,
#' # `a` is recoded to `1.0`, `b` maps to `1.5`, and
#' # all other values become `0.5`
#' graph <-
#'   graph %>%
#'   recode_edge_attrs(
#'     "rel",
#'     "a" = 1.0,
#'     "b" = 1.5,
#'     otherwise = 0.5,
#'     edge_attr_to = "penwidth")
#'
#' # Get the graph's internal edf to show that the
#' # node attribute values had been recoded and
#' # copied into a new node attribute
#' get_edge_df(graph)
#' #>   from to rel penwidth
#' #> 1    3  2   a        1
#' #> 2    1  2   b      1.5
#' #> 3    4  3   a        1
#' #> 4    1  4   c      0.5
#' #> 5    1  3   b      1.5
#' #> 6    2  4   d      0.5
#' @export recode_edge_attrs

recode_edge_attrs <- function(graph,
                              edge_attr_from,
                              ...,
                              otherwise = NULL,
                              edge_attr_to = NULL) {

  # Get list object from named vectors
  replacements <- list(...)

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's edge attributes
  if (!any(column_names_graph %in% edge_attr_from)) {
    stop("The edge attribute to recode is not in the edf.")
  }

  # Get the column number for the edge attr to recode
  col_num_recode <-
    which(colnames(edges) %in% edge_attr_from)

  # Extract the vector to recode from the `edges` df
  vector_to_recode <-
    edges[, col_num_recode]

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
      which(!(1:nrow(edges) %in% unlist(indices)))

    if (length(otherwise_indices) > 0) {
      vector_to_recode[otherwise_indices] <-
        otherwise
    }
  }

  # Coerce the recoded vector to a character vector
  recoded_vector <-
    as.character(vector_to_recode)

  if (!is.null(edge_attr_to)) {

    # Stop function if `edge_attr_to` is
    # `from` or `to`
    if (any(c("from", "to") %in% edge_attr_to)) {
      stop("You cannot use those names.")
    }

    if (any(column_names_graph %in% edge_attr_to)) {

      # Edge attribute exists and values will be
      # overwritten in this case
      col_num_write_to <-
        which(column_names_graph %in% edge_attr_to)

      edges[, col_num_write_to] <-
        recoded_vector
    }

    if (!any(column_names_graph %in% edge_attr_to)) {

      # Edge attribute does not exist and values be
      # part of a new edge attribute
      edges <-
        cbind(edges, data.frame(recoded_vector))

      # Set the column name for the copied attr
      colnames(edges)[ncol(edges)] <- edge_attr_to
    }
  } else {
    # The edge attribute values will be overwritten
    # by the recoded value (no new edge attrs)
    edges[, col_num_recode] <- recoded_vector
  }

  # Create a new graph object
  dgr_graph <-
    create_graph(nodes_df = graph$nodes_df,
                 edges_df = edges,
                 graph_attrs = graph$graph_attrs,
                 node_attrs = graph$node_attrs,
                 edge_attrs = graph$edge_attrs,
                 directed = graph$directed,
                 graph_name = graph$graph_name,
                 graph_time = graph$graph_time,
                 graph_tz = graph$graph_tz)

  return(dgr_graph)
}
