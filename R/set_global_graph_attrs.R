#' Set global graph attributes
#' @description Set global attributes of a specific
#' type (either \code{graph_attrs}, \code{node_attrs},
#' or \code{edge_attrs} for a graph object of class
#' \code{dgr_graph}).
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param attr the name of the attribute to
#' set for the \code{type} of global attribute
#' specified.
#' @param value the value to be set for the chosen
#' attribute specified in the \code{attr_for_type}
#' argument.
#' @param attr_type the specific type of global graph
#' attribute to set. The type is specified with
#' \code{graph}, \code{node}, or \code{edge}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a new graph and set some global attributes
#' graph <- create_graph() %>%
#'   set_global_graph_attrs(
#'     "overlap", "true", "graph")
#'
#' # Verify that the global attributes have been set
#' get_global_graph_attrs(graph)
#' #>      attr value attr_type
#' #> 1 overlap  true     graph
#' @importFrom tibble tibble
#' @export set_global_graph_attrs

set_global_graph_attrs <- function(graph,
                                   attr,
                                   value,
                                   attr_type) {

  # Coerce any logical value for `value` to a
  # lowercase character value
  if (inherits(value, "logical") & value %in% c(TRUE, FALSE)) {
    value <- tolower(as.character(value))
  }

  # Create a table for the attributes
  global_attrs <-
    tibble::tibble(
      attr = as.character(attr),
      value = as.character(value),
      attr_type = as.character(attr_type)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  graph$global_attrs <- global_attrs

  return(graph)
}
