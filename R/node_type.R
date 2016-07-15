#' Create, read, update, delete, or report status of a
#' node type definition
#' @description From a graph object of class
#' \code{dgr_graph}, query a node in the graph (using
#' the node ID) and perform operations on the type
#' definition for that node.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node a node ID corresponding to the node to
#' be selected.
#' @param action the operation to perform on the node's
#' type attribute. To remove the type definition from a
#' node, use either \code{delete}, \code{remove}, or
#' \code{drop}. To add a type definition to a node with
#' no type set, use \code{add} or \code{create}. To
#' update a node's type definition, use \code{update}.
#' To return the value of a node type, use \code{read}.
#' To determine whether there is a type set for the
#' selected node, use \code{check}.
#' @param value a string denoting the node type, to be
#' supplied when either adding or updating a node type
#' definition.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' library(magrittr)
#'
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_nodes(
#'     nodes = 1:5,
#'     type = c("a", "b", "c", "a", "c"))
#'
#' # Create an edge data frame (edf)
#' edges <-
#'   create_edges(
#'     from = c(1, 3, 5, 2, 4),
#'     to = c(2, 2, 4, 4, 3))
#'
#' # Create a graph
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Read the node `type` for node `1`
#' graph %>%
#'   node_type(1)
#' #> [1] "a"
#'
#' # Remove the `type` value entirely from
#' # node `1`
#' graph %<>%
#'   node_type(1, "delete")
#'
#' # Check that node `1` no longer has a
#' # `type` assignment
#' graph %>%
#'   node_type(1, "check")
#' #> [1] FALSE
#'
#' # Add the `type` value "b" to node `1`
#' graph %<>%
#'   node_type(1, "add", "b")
#'
#' # Read the node `type` for node `1`
#' graph %>%
#'   node_type(1)
#' #> [1] "b"
#'
#' # Perform an in-place update of the `type`
#' # value for node `1` ("b" to "a")
#' graph %<>%
#'   node_type(1, "update", "a")
#'
#' # Read the node `type` for node `1` to ensure
#' # that the change was made
#' graph %>%
#'   node_type(1)
#' #> [1] "a"
#' @export node_type

node_type <- function(graph,
                      node,
                      action = "read",
                      value = NULL) {


  # Determine if node is present within the graph
  node_is_in_graph <-
    node_present(graph = graph, node = node)

  # Stop function if node is not present within
  # the graph
  if (node_is_in_graph == FALSE) {
    stop("The specified node is not present in the graph.")
  }

  if (node_is_in_graph) {
    node_row <- which(graph$nodes_df$nodes == node)

    type_set <-
      ifelse(graph$nodes_df$type[node_row] == "",
             FALSE, TRUE)

    # Remove type if a `type` value is set
    if (action %in% c("delete", "remove", "drop")) {
      if (type_set == FALSE) {
        return(graph)
      }

      if (type_set) {
        graph$nodes_df$type[node_row] <- ""
        return(graph)
      }
    }

    # Add a `type` to a node with no `type`
    # definition set
    if (action %in% c("add", "create")) {
      if (type_set) {
        return(graph)
      }
      if (type_set == FALSE & !is.null(value)) {
        graph$nodes_df$type[node_row] <- value
        return(graph)
      }
    }

    # Update an existing `type` definition for a node
    if (action == "update") {
      if (type_set == FALSE) {
        return(graph)
      }
      if (type_set & !is.null(value)) {
        graph$nodes_df$type[node_row] <- value
        return(graph)
      }
    }

    # Return the value of an existing node `type`
    if (action == "read") {
      if (type_set == FALSE) {
        return(NA)
      }
      if (type_set) {
        type_value <- graph$nodes_df$type[node_row]
        return(type_value)
      }
    }

    # Determine whether a node `type` definition has
    # been set
    if (action == "check") {
      if (type_set == FALSE) {
        return(FALSE)
      }

      if (type_set) {
        return(TRUE)
      }
    }
  }
}
