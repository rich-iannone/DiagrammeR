#' Get node IDs associated with edges
#'
#' Obtain a vector, data frame, or list of node IDs associated with edges in a
#'   graph object. An optional filter by edge attribute can limit the set of
#'   edges returned.
#' @inheritParams render_graph
#' @param conditions an option to use filtering conditions for the retrieval of
#'   edges.
#' @param return_type using \code{vector} (the default), a vector of character
#'   objects representing the edges is provided. With \code{list} a list object
#'   will be provided that contains vectors of outgoing and incoming node IDs
#'   associated with edges. With \code{df}, a data frame containing outgoing and
#'   incoming node IDs associated with edges.
#' @param return_values using \code{id} (the default) results in node ID values
#'   returned in the edge definitions. With \code{label}, the node labels will
#'   instead be used to define edges.
#' @return a list, data frame, or a vector object, depending on the value given
#'   to \code{return_type}.
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
#' graph %>%
#'   get_edges(
#'     return_type = "vector")
#'
#' # Get all edges within a graph, returned as a
#' # data frame
#' graph %>%
#'   get_edges(
#'     return_type = "df")
#'
#' # Get all edges returned as a list
#' graph %>%
#'   get_edges(
#'     return_type = "list")
#'
#' # Get a vector of edges using
#' # a numeric comparison (i.e.,
#' # all edges with a `value`
#' # attribute greater than 3)
#' graph %>%
#'   get_edges(
#'     conditions = value > 3,
#'     return_type = "vector")
#'
#' # Get a vector of edges using
#' # a matching condition
#' graph %>%
#'   get_edges(
#'     conditions = color == "pink",
#'     return_type = "vector")
#'
#' # Use multiple conditions to
#' # return edges with the
#' # desired attribute values
#' graph %>%
#'   get_edges(
#'     conditions =
#'       color == "blue" &
#'       value > 3,
#'     return_type = "vector")
#'
#' # Use `return_values = "label"`
#' # to return the labels of the
#' # connected nodes
#' graph %>%
#'   get_edges(
#'     conditions =
#'       color == "blue" &
#'       value > 3,
#'     return_type = "vector",
#'     return_values = "label")
#' @importFrom dplyr filter select_ left_join rename
#' @importFrom rlang enquo UQ get_expr
#' @export
get_edges <- function(graph,
                      conditions = NULL,
                      return_type = "vector",
                      return_values = "id") {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Capture provided conditions
  conditions <- rlang::enquo(conditions)

  # Create bindings for specific variables
  label <- NULL

  # Extract edge data frame from the graph
  edges_df <- graph$edges_df

  if (return_values == "label") {
    edges_df <-
      edges_df %>%
      dplyr::left_join(graph$nodes_df %>% select_("id", "label"), by = c("from" = "id")) %>%
      dplyr::rename(from_label_ = label) %>%
      dplyr::left_join(graph$nodes_df %>% select_("id", "label"), by = c("to" = "id")) %>%
      dplyr::rename(to_label_ = label)
  }


  # If conditions are provided then
  # pass in those conditions and filter the
  # data frame of `edges_df`
  if (!is.null(
    rlang::enquo(conditions) %>%
    rlang::get_expr())) {

    edges_df <-
      filter(
        .data = edges_df,
        rlang::UQ(conditions))
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
