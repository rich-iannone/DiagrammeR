#' Mutate a set of edge attribute values
#' @description Within a graph's internal edge data
#' frame (edf), mutate numeric edge attribute values
#' using an expression. Optionally, one can specify a
#' different edge attribute name and create a new edge
#' attribute while retaining the original edge
#' attribute and its values.
#' @param graph a graph object of class
#' @param edge_attr_from the name of the edge attribute
#' column from which values will be mutated.
#' @param expression an expression for the mutation of
#' all edge attribute values specified by
#' \code{edge_attr_from}. It is to be supplied as a
#' string where a \code{.} represents the edge
#' attribute values.
#' @param edge_attr_to an optionally supplied name of
#' a new edge attribute to which the mutated values
#' will be applied. This will retain the original edge
#' attribute and its values.
#' @param round_to the maximum number of decimal places
#' to retain for the mutated edge attribute values. The
#' default value is \code{5}.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 6, set_seed = 3) %>%
#'     set_edge_attrs("value", 3)
#'
#' # Get the graph's internal edf to show which
#' # edge attributes are available
#' get_edge_df(graph)
#' #>   from to rel value
#' #> 1    4  1         3
#' #> 2    2  4         3
#' #> 3    4  3         3
#' #> 4    3  5         3
#' #> 5    5  1         3
#' #> 6    4  5         3
#'
#' # Mutate the `value` edge attribute, multiplying
#' # each value by 1.5
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs("value", ". * 1.5")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute values had been mutated
#' get_edge_df(graph)
#' #>   from to rel value
#' #> 1    4  1       4.5
#' #> 2    2  4       4.5
#' #> 3    4  3       4.5
#' #> 4    3  5       4.5
#' #> 5    5  1       4.5
#' #> 6    4  5       4.5
#'
#' # Create a new edge attribute, called `penwidth`,
#' # that is the log of values in `value` plus 1
#' graph <-
#'   graph %>%
#'   mutate_edge_attrs(
#'     "value", "log(.) + 1", "penwidth")
#'
#' # Get the graph's internal edf to show that the
#' # edge attribute values had been mutated and used as
#' # the new edge attribute `penwidth`
#' get_edge_df(graph)
#' #>   from to rel value penwidth
#' #> 1    4  1       4.5  2.50408
#' #> 2    2  4       4.5  2.50408
#' #> 3    4  3       4.5  2.50408
#' #> 4    3  5       4.5  2.50408
#' #> 5    5  1       4.5  2.50408
#' #> 6    4  5       4.5  2.50408
#' @export mutate_edge_attrs

mutate_edge_attrs <- function(graph,
                              edge_attr_from,
                              expression,
                              edge_attr_to = NULL,
                              round_to = 5) {

  # Extract the graph's edf
  edges <- get_edge_df(graph)

  # Get column names from the graph's edf
  column_names_graph <- colnames(edges)

  # Stop function if `edge_attr_from` is not one
  # of the graph's edge attributes
  if (!any(column_names_graph %in% edge_attr_from)) {
    stop("The edge attribute to mutate is not in the edf.")
  }

  # Get the column number for the edge attr to copy
  col_num_evaluate <-
    which(colnames(edges) %in% edge_attr_from)

  # If edge attribute is not numeric, stop function
  if (is.na(suppressWarnings(
    any(as.numeric(edges[,col_num_evaluate]))))) {
    stop("The edge attribute to mutate is not numeric.")
  }

  # Extract the vector to evaluate from the `edges` df
  vector_to_eval <-
    as.numeric(edges[, col_num_evaluate])

  parsed_expression <-
    gsub("([^0-9])(\\.)([^0-9])",
         "\\1vector_to_eval\\3", expression)

  parsed_expression <-
    gsub("(^\\.|\\.$)",
         "vector_to_eval", parsed_expression)

  # Evaluate the parsed expression
  mutated_vector <-
    eval(parse(text = parsed_expression))

  # Round and coerce the mutated vector
  mutated_vector <-
    as.character(round(mutated_vector, round_to))

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

      edges[, col_num_write_to] <- mutated_vector
    }

    if (!any(column_names_graph %in% edge_attr_to)) {

      # Edge attribute does not exist and values be
      # part of a new edge attribute
      edges <-
        cbind(edges, data.frame(mutated_vector))

      # Set the column name for the copied attr
      colnames(edges)[ncol(edges)] <- edge_attr_to
    }
  } else {
    # The edge attribute values will be overwritten
    # by the mutated value (no new edge attrs)
    edges[, col_num_evaluate] <- mutated_vector
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
