#' Cache node attributes (based on a selection of
#' nodes) in the graph
#' @description From a graph object of class
#' \code{dgr_graph}, get node attribute properties for
#' nodes available in a selection and cache those
#' values in the graph for later retrieval using
#' \code{get_cache}.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node_attr the node attribute from which to
#' obtain values.
#' @param mode a option to recast the returned vector
#' of node attribute value as \code{numeric} or
#' \code{character}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Set a seed
#' set.seed(25)
#'
#' # Create a graph with 10 nodes and 9 edges
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(10) %>%
#'   set_node_attrs(
#'     "value", rnorm(node_count(.), 5, 2)) %>%
#'   add_edges_w_string(
#'     "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")
#'
#' # Select all nodes where the node attribute `value`
#' # is less than 5
#' graph <-
#'   graph %>%
#'   select_nodes(
#'     node_attr = "value",
#'     search = "<5.0")
#'
#' # Cache available values from the node attribute
#' # `value` from the nodes that are selected; ensure
#' # that the cached vector is numeric
#' graph <-
#'   graph %>%
#'   cache_node_attrs_ws("value", "numeric")
#'
#' # Get the cached vector and get its
#' # difference from 5
#' graph %>% get_cache %>% {x <- .; 5 - x}
#' #> [1] 0.4236672 2.0831823 2.3066151 3.0002598
#' #> [5] 0.8910665 0.1157822
#' @export cache_node_attrs_ws

cache_node_attrs_ws <- function(graph,
                                node_attr,
                                mode = NULL) {

  if (is.null(graph$selection$nodes)) {
    stop("There is no selection of nodes available.")
  }

  nodes_df <-
    get_node_df(graph)[which(get_node_df(graph)[,1]
                             %in% graph$selection$nodes),]

  if (any(node_attr %in%
          colnames(nodes_df)[-1])) {

    nodes_attr_vector <-
      nodes_df[,which(colnames(nodes_df) %in%
                        node_attr)]

    if (!is.null(mode)) {
      if (mode == "numeric") {
        nodes_attr_vector <-
          as.numeric(nodes_attr_vector)

        nodes_attr_vector <-
          nodes_attr_vector[which(!is.na(nodes_attr_vector))]
      }
      if (mode == "character") {
        nodes_attr_vector <- as.character(nodes_attr_vector)
      }
    }

    # Cache vector of node attributes
    # in the graph
    graph$cache <- nodes_attr_vector

    return(graph)
  }
}
