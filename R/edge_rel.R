#' Create, read, update, delete, or report status of an
#' edge relationship
#' @description From a graph object of class
#' \code{dgr_graph}, query an edge in the graph
#' (defined by a pair of node IDs extant in the graph)
#' and perform operations on the relationship for that
#' edge.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param from a node ID from which the edge to be
#' queried is outgoing.
#' @param to a node ID to which the edge to be queried
#' is incoming.
#' @param action the operation to perform on the edge's
#' relationship attribute. To remove a relationship
#' from an edge, use either \code{delete},
#' \code{remove}, or \code{drop}. To add a relationship
#' to an edge with no set relationship, use \code{add}
#' or \code{create}. To update an edge relationship,
#' use \code{update}. To return the value of an edge
#' relationship, use \code{read}. To determine whether
#' there is a set relationship, use \code{check}.
#' @param value a string denoting the relationship, to
#' be supplied when either adding or updating an edge
#' relationship.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 5,
#'     type = c("a", "b", "c", "a", "c"))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 3, 5, 2, 4),
#'     to = c(2, 2, 4, 4, 3),
#'     rel = c("rel_a", "rel_b",
#'             "rel_b", "rel_a",
#'             "rel_c"))
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Read the edge `rel` for edge `1`->`2`
#' graph %>% edge_rel(1, 2)
#' #> [1] "rel_a"
#'
#' # Remove the `rel` value entirely from
#' # edge `1`->`2`
#' graph <-
#'   graph %>%
#'   edge_rel(
#'     from = 1,
#'     to = 2,
#'     action = "delete")
#'
#' # Check that the edge `1`->`2` no longer
#' # has a `rel` assignment
#' graph %>%
#'   edge_rel(
#'     from = 1,
#'     to = 2,
#'     action = "check")
#' #> [1] FALSE
#'
#' # Add the `rel` value `rel_b`` to edge `1`->`2`
#' graph <-
#'   graph %>%
#'   edge_rel(
#'     from = 1,
#'     to = 2,
#'     action = "add",
#'     value = "rel_b")
#'
#' # Read the edge `rel` for edge `1`->`2`
#' graph %>%
#'   edge_rel(
#'     from = 1,
#'     to = 2)
#' #> [1] "rel_b"
#'
#' # Perform an in-place update of the `rel`
#' # value for edge `1`->`2` (`rel_b`` to `rel_a``)
#' graph <-
#'   graph %>%
#'   edge_rel(
#'     from = 1,
#'     to = 2,
#'     action = "update",
#'     value = "rel_a")
#'
#' # Read the edge `rel` for edge `1`->`2`
#' # to ensure that the change was made
#' graph %>%
#'   edge_rel(
#'     from = 1,
#'     to = 2)
#' #> [1] "rel a"
#' @export edge_rel

edge_rel <- function(graph,
                     from,
                     to,
                     action = "read",
                     value = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Determine if edge is present within the graph
  edge_is_in_graph <-
    edge_present(
      graph = graph,
      from = from,
      to = to)

  # Stop function if edge is not present within
  # the graph
  if (edge_is_in_graph == FALSE) {
    stop("The specified edge is not present in the graph.")
  }

  if (edge_is_in_graph) {

    edge_row <-
      which(graph$edges_df$from == from &
              graph$edges_df$to == to)

    relationship_set <-
      ifelse(is.null(graph$edges_df$rel[edge_row]) ||
               is.na(graph$edges_df$rel[edge_row]),
             FALSE, TRUE)

    # Remove relationship if a relationship is set
    if (action %in% c("delete", "remove", "drop")) {
      if (relationship_set == FALSE) {
        return(graph)
      }
      if (relationship_set) {
        graph$edges_df$rel[edge_row] <- as.character(NA)
        return(graph)
      }
    }

    # Add a relationship to an edge with no set
    # relationship
    if (action %in% c("add", "create")) {
      if (relationship_set) {
        return(graph)
      }
      if (relationship_set == FALSE &
          !is.null(value)) {
        if (is.null(graph$edges_df$rel)) {

          rel_col <-
            vector(mode = "character",
                 length = nrow(graph$edges_df))

          rel_col[edge_row] <- value
          graph$edges_df$rel <- rel_col
        }
        if (!is.null(graph$edges_df$rel)) {
          graph$edges_df$rel[edge_row] <- value
        }
        return(graph)
      }
    }

    # Update an existing relationship for an edge
    if (action == "update") {
      if (relationship_set == FALSE) {
        return(graph)
      }
      if (relationship_set
          & !is.null(value)) {
        graph$edges_df$rel[edge_row] <- value
        return(graph)
      }
    }

    # Return the value of an existing relationship
    # for an edge
    if (action == "read") {
      if (relationship_set == FALSE) {
        relationship_value <- NA
        return(relationship_value)
      }
      if (relationship_set) {
        relationship_value <-
          graph$edges_df$rel[edge_row]
        return(relationship_value)
      }
    }

    # Determine whether a relationship has been set
    if (action == "check") {
      if (relationship_set == FALSE) {
        return(FALSE)
      }
      if (relationship_set) {
        return(TRUE)
      }
    }
  }
}
