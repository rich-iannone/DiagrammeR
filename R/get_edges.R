#' Get node IDs associated with edges
#' @description Obtain a vector, data frame, or list of
#' node IDs from a graph object or an edge data frame.
#' An optional filter by edge attribute can limit the
#' set of edges returned.
#' @param x either a graph object of class
#' \code{dgr_graph} or an edge data frame.
#' @param conditions an option to use filtering
#' conditions for the retrieval of edges.
#' @param return_type using \code{vector} (the
#' default), a vector of character objects
#' representing the edges is provided. With
#' \code{list} a list object will be provided that
#' contains vectors of outgoing and incoming node IDs
#' associated with edges. With \code{df}, a data frame
#' containing outgoing and incoming node IDs associated
#' with edges.
#' @param return_values using \code{id} (the default)
#' results in node ID values returned in the edge
#' definitions. With \code{label}, the node labels will
#' instead be used to define edges.
#' @return a list, data frame, or a vector object,
#' depending on the value given to \code{return_type}.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     label = c("one", "two", "three", "four"),
#'     type = "letter",
#'     color = c("red", "green", "grey", "blue"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to",
#'     color = c("pink", "blue", "blue"),
#'     value = c(3.9, 2.5, 7.3))
#'
#' # Create a graph
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get all edges within a graph, returned as a list
#' get_edges(graph, return_type = "vector")
#' #> [1] "1->4" "2->3" "3->1"
#'
#' # Get all edges within a graph, returned as a
#' # data frame
#' get_edges(graph, return_type = "df")
#' #>   from to
#' #> 1    1  4
#' #> 2    2  3
#' #> 3    3  1
#'
#' # Get all edges returned as a list
#' get_edges(graph, return_type = "list")
#' #> [[1]]
#' #> [1] 1 2 3
#' #>
#' #> [[2]]
#' #> [1] 4 3 1
#'
#' # Get a vector of edges using a numeric
#' # comparison (i.e., all edges with a `value`
#' # attribute greater than 3)
#' get_edges(
#'   graph,
#'   conditions = "value > 3",
#'   return_type = "vector")
#' #> [1] "1->4" "3->1"
#'
#' # Get a vector of edges using a match
#' get_edges(
#'   graph,
#'   conditions = "color == 'pink'",
#'   return_type = "vector")
#' #> [1] "1->4"
#'
#' # Use multiple conditions to return edges
#' # with the desired attribute values
#' get_edges(
#'   graph,
#'   conditions =
#'     c("color == 'blue'",
#'       "value > 3"),
#'   return_type = "vector")
#' #> [1] "3->1"
#'
#' # Use `return_values = "label"` to return
#' # the labels of the connected nodes
#' get_edges(
#'   graph,
#'   conditions =
#'     c("color == 'blue'",
#'       "value > 3"),
#'   return_type = "vector",
#'   return_values = "label")
#' #> [1] "three->one"
#' @importFrom dplyr filter_ select_ left_join rename
#' @export get_edges

get_edges <- function(x,
                      conditions = NULL,
                      return_type = "vector",
                      return_values = "id") {

  # Create bindings for specific variables
  label <- NULL

  if (inherits(x, "dgr_graph")) {
    edges_df <- x$edges_df

    if (return_values == "label") {
      edges_df <-
        edges_df %>%
        dplyr::left_join(x$nodes_df %>% select_("id", "label"), by = c("from" = "id")) %>%
        dplyr::rename(from_label_ = label) %>%
        dplyr::left_join(x$nodes_df %>% select_("id", "label"), by = c("to" = "id")) %>%
        dplyr::rename(to_label_ = label)
    }
  }

  if (inherits(x, "data.frame")) {
    if (colnames(x)[1] == "id" &
        colnames(x)[2] == "from" &
        colnames(x)[3] == "to" &
        colnames(x)[4] == "rel") {
      edges_df <- x
    } else {
      stop("The supplied object is not an edge data frame.")
    }
  }

  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `edges_df`
  if (!is.null(conditions)) {
    for (i in 1:length(conditions)) {
      edges_df <-
        edges_df %>%
        dplyr::filter_(conditions[i])
    }
  }

  # If no edges remain then return NA
  if (nrow(edges_df) == 0) {
    return(NA)
  }

  if (return_type == "list") {

    edges_list <- vector(mode = "list")
    edges_list[[1]] <- edges_list[[2]] <- vector(mode = "integer")

    if (return_values == "id") {
      edges_list[[1]] <- c(edges_list[[1]], edges_df$from)
      edges_list[[2]] <- c(edges_list[[2]], edges_df$to)
    } else if (return_values == "label") {
      edges_list[[1]] <- c(edges_list[[1]], edges_df$from_label_)
      edges_list[[2]] <- c(edges_list[[2]], edges_df$to_label_)
    }

    return(edges_list)
  }

  if (return_type == "df") {

    if (return_values == "id") {
      edges_df <-
        edges_df %>%
        dplyr::select_("from", "to")
    } else if (return_values == "label") {
      edges_df <-
        edges_df %>%
        dplyr::select_("from_label_", "to_label_")
    }

    return(edges_df)
  }

  if (return_type == "vector") {

    if (return_values == "id") {
      edges_vector <-
        paste0(edges_df$from, "->", edges_df$to)
    } else if (return_values == "label") {
      edges_vector <-
        paste0(edges_df$from_label_, "->", edges_df$to_label_)
    }

    return(edges_vector)
  }
}
