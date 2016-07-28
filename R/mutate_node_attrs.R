#' Mutate a set of node attribute values
#' @description Within a graph's internal node data
#' frame (ndf), mutate numeric node attribute values
#' using an expression. Optionally, one can specify a
#' different node attribute name and create a new node
#' attribute while retaining the original node
#' attribute and its values.
#' @param graph a graph object of class
#' @param node_attr_from the name of the node attribute
#' column from which values will be mutated.
#' @param expression an expression for the mutation of
#' all node attribute values specified by
#' \code{node_attr_from}. It is to be supplied as a
#' string where a \code{.} represents the node
#' attribute values.
#' @param node_attr_to an optionally supplied name of
#' a new node attribute to which the mutated values
#' will be applied. This will retain the original node
#' attribute and its values.
#' @param round_to the maximum number of decimal places
#' to retain for the mutated node attribute values. The
#' default value is \code{5}.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a random graph
#' graph <-
#'   create_random_graph(
#'     5, 10, set_seed = 3)
#'
#' # Get the graph's internal ndf to show which
#' # node attributes are available
#' get_node_df(graph)
#' #>   nodes type label value
#' #> 1     1          1     2
#' #> 2     2          2   8.5
#' #> 3     3          3     4
#' #> 4     4          4   3.5
#' #> 5     5          5   6.5
#'
#' # Mutate the `value` node attribute, dividing each
#' # value by 2
#' graph <-
#'   graph %>%
#'   mutate_node_attrs("value", ". / 2")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute values had been mutated
#' get_node_df(graph)
#' #>   nodes type label value
#' #> 1     1          1     1
#' #> 2     2          2  4.25
#' #> 3     3          3     2
#' #> 4     4          4  1.75
#' #> 5     5          5  3.25
#'
#' # Create a new node attribute, called `width`,
#' # that is the log of values in `value` plus 1
#' graph <-
#'   graph %>%
#'   mutate_node_attrs("value", "log(.) + 1", "width")
#'
#' # Get the graph's internal ndf to show that the
#' # node attribute values had been mutated and used as
#' # the new node attribute `width`
#' get_node_df(graph)
#' #>   nodes type label value   width
#' #> 1     1          1     1       1
#' #> 2     2          2  4.25 2.44692
#' #> 3     3          3     2 1.69315
#' #> 4     4          4  1.75 1.55962
#' #> 5     5          5  3.25 2.17865
#' @export mutate_node_attrs

mutate_node_attrs <- function(graph,
                              node_attr_from,
                              expression,
                              node_attr_to = NULL,
                              round_to = 5) {

  # Extract the graph's ndf
  nodes <- get_node_df(graph)

  # Get column names from the graph's ndf
  column_names_graph <- colnames(nodes)

  # Stop function if `node_attr_from` is not one
  # of the graph's node attributes
  if (!any(column_names_graph %in% node_attr_from)) {
    stop("The node attribute to mutate is not in the ndf.")
  }

  # Get the column number for the node attr to copy
  col_num_evaluate <-
    which(colnames(nodes) %in% node_attr_from)

  # If node attribute is not numeric, stop function
  if (is.na(suppressWarnings(
    any(as.numeric(nodes[, col_num_evaluate]))))){
    stop("The node attribute to mutate is not numeric.")
  }

  # Extract the vector to evaluate from the `nodes` df
  vector_to_eval <-
    as.numeric(nodes[, col_num_evaluate])

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

      nodes[,col_num_write_to] <-
        mutated_vector
    }

    if (!any(column_names_graph %in% node_attr_to)) {

      # Node attribute does not exist and values be
      # part of a new node attribute
      nodes <-
        cbind(nodes, data.frame(mutated_vector))

      # Set the column name for the copied attr
      colnames(nodes)[ncol(nodes)] <- node_attr_to
    }
  } else {
    # The node attribute values will be overwritten
    # by the mutated value (no new node attrs)
    nodes[, col_num_evaluate] <- mutated_vector
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
