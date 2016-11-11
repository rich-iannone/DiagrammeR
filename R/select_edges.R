#' Select edges in a graph
#' @description Select edges from a graph object of
#' class \code{dgr_graph}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edge_attr an optional character vector of
#' edge attribute values for filtering the edges
#' returned.
#' @param search an option to provide a logical
#' expression with a comparison operator (\code{>},
#' \code{<}, \code{==}, or \code{!=}) followed by a
#' number for numerical filtering, or, a regular
#' expression for filtering the nodes returned through
#' string matching.
#' @param set_op the set operation to perform upon
#' consecutive selections of graph nodes. This can
#' either be as a \code{union} (the default), as an
#' intersection of selections with \code{intersect},
#' or, as a \code{difference} on the previous
#' selection, if it exists.
#' @param from an optional vector of node IDs from
#' which the edge is outgoing for filtering the list of
#' edges present in the graph.
#' @param to an optional vector of node IDs to which
#' the edge is incoming for filtering the list of
#' edges present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("a", "z", "a"),
#'     value = c(6.4, 2.9, 5.0))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Explicitly select the edge `1` -> `4`
#' graph <-
#'   graph %>%
#'   select_edges(from = 1, to = 4)
#'
#' # Verify that an edge selection has been made
#' # using the `get_selection()` function
#' get_selection(graph)
#' #> [1] "1 -> 4"
#'
#' # Select edges based on the relationship label
#' # being `z`
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges(edge_attr = "rel", search = "z")
#'
#' # Verify that an edge selection has been made, and
#' # recall that the `2` -> `3` edge uniquely has the
#' # `z` relationship label
#' get_selection(graph)
#' #> [1] "2 -> 3"
#'
#' # Select edges based on the edge value attribute
#' # being greater than 3.0 (first clearing the current
#' # selection of edges)
#' graph <-
#'   graph %>%
#'   clear_selection() %>%
#'   select_edges(edge_attr = "value", search = ">3.0")
#'
#' # Verify that the correct edge selection has been
#' # made; in this case, edges `1` -> `4` and
#' # `3` -> `1` have values for `value` greater than
#' # 3.0
#' get_selection(graph)
#' #> [1] "1 -> 4" "3 -> 1"
#' @export select_edges

select_edges <- function(graph,
                         edge_attr = NULL,
                         search = NULL,
                         set_op = "union",
                         from = NULL,
                         to = NULL) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Validation: Graph contains nodes
  if (graph_contains_nodes(graph) == FALSE) {
    stop("The graph contains no nodes, so, no selections can be made.")
  }

  # Validation: Graph contains edges
  if (graph_contains_edges(graph) == FALSE) {
    stop("The graph contains no edges, so, no selections can be made.")
  }

  # Remove any selection of nodes
  graph$selection$nodes <- NULL

  # Remove `graph$selection` if empty
  if (length(graph$selection) == 0) {
    graph$selection <- NULL
  }

  # Extract the graph's internal edf
  edges_df <- graph$edges_df

  if (!is.null(edge_attr)) {
    if (length(edge_attr) > 1) {
      stop("Only one edge attribute can be specified.")
    }

    if (!(edge_attr %in% colnames(edges_df)[-(1:2)])) {
      stop("The specified attribute is not available.")
    }
  }

  if (is.null(edge_attr)) {
    if (is.null(from) & !is.null(to)) {
      if (any(!(to %in% edges_df$to))) {
        stop("One of more of the incoming nodes specified are not part of an edge.")
      }

      edges_selected <-
        get_edges(edges_df[which(edges_df$to %in% to),],
                  return_type = "vector")

    } else if (!is.null(from) & is.null(to)) {
      if (any(!(from %in% edges_df$from))) {
        stop("One of more of the outgoing nodes specified are not part of an edge.")
      }

      edges_selected <-
        get_edges(edges_df[which(edges_df$from %in% from),],
                  return_type = "vector")
    } else if (is.null(from) & is.null(to)) {
      edges_selected <-
        get_edges(edges_df, return_type = "vector")
    } else {
      edges_selected <-
        get_edges(
          edges_df[which((edges_df$from %in% from) &
                           (edges_df$to %in% to)),],
          return_type = "vector")
    }
  }

  if (!is.null(edge_attr)) {

    column_number <-
      which(colnames(edges_df) %in% edge_attr)

    # If a search term provided, filter using a logical expression
    # or a regex match
    if (!is.null(search)) {
      if (grepl("^>.*", search) | grepl("^<.*", search) |
          grepl("^==.*", search) | grepl("^!=.*", search)) {
        logical_expression <- TRUE } else {
          logical_expression <- FALSE
        }

      # Filter using a logical expression
      if (logical_expression) {
        if (grepl("^>.*", search)) {
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) >
                    as.numeric(gsub(">(.*)", "\\1", search)))
        }

        if (grepl("^<.*", search)) {
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) <
                    as.numeric(gsub("<(.*)", "\\1", search)))
        }

        if (grepl("^==.*", search)) {
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) ==
                    as.numeric(gsub("==(.*)", "\\1", search)))
        }

        if (grepl("^!=.*", search)) {
          rows_where_true_le <-
            which(as.numeric(edges_df[,column_number]) !=
                    as.numeric(gsub("!=(.*)", "\\1", search)))
        }

        edges_selected <-
          get_edges(edges_df[rows_where_true_le, ],
                    return_type = "vector")
      }

      # Filter using a `search` value as a
      # regular expression
      if (logical_expression == FALSE) {

        rows_where_true_regex <-
          which(
            grepl(search,
                  as.character(
                    edges_df[,column_number])))

        edges_selected <-
          get_edges(
            edges_df[rows_where_true_regex, ],
            return_type = "vector")
      }
    }
  }

  # Obtain vectors of node IDs associated with edges
  # already present
  if (!is.null(graph$selection)) {
    if (!is.null(graph$selection$edges)) {
      from_prev_selection <- graph$selection$edges$from
      to_prev_selection <- graph$selection$edges$to

      edges_prev_selection <-
        sapply(1:length(from_prev_selection),
               function(x) paste(from_prev_selection[x],
                                 "->",
                                 to_prev_selection[x]))
    }
  } else {
    edges_prev_selection <- vector(mode = "character")
  }

  # Incorporate the selected edges into the
  # graph's selection
  if (set_op == "union") {
    edges_combined <-
      union(edges_prev_selection, edges_selected)
  } else if (set_op == "intersect") {
    edges_combined <-
      intersect(edges_prev_selection, edges_selected)
  } else if (set_op == "difference") {
    edges_combined <-
      setdiff(edges_prev_selection, edges_selected)
  }

  from_combined <-
    gsub("\\s", "",
         gsub("(.*)(->|--)(.*)",
              "\\1", edges_combined))

  to_combined <-
    gsub("\\s", "",
         gsub("(.*)(->|--)(.*)",
              "\\3", edges_combined))

  # Create selection of edges
  graph$selection$edges$from <- as.integer(from_combined)
  graph$selection$edges$to <- as.integer(to_combined)

  return(graph)
}
