#' Get edge attributes based on a selection of edges
#' @description From a graph object of class \code{dgr_graph}, get edge
#' attribute properties for edges available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param edge_attr the edge attribute from which to obtain values.
#' @param mode a option to recast the returned vector of edge attribute
#' value as \code{numeric} or \code{character}.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#'
#' # Create a simple graph
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                value = c(3.5, 2.6, 9.4, 2.7))
#'
#' edges <-
#'   create_edges(from = c("a", "b", "c"),
#'                to = c("d", "c", "a"),
#'                rel = "leading_to",
#'                color = c("pink", "blue", "red"))
#'
#' graph <-
#'   create_graph(nodes_df = nodes,
#'                edges_df = edges)
#'
#' # Select edges "b" -> "c" and "a" -> "d" in the graph and get
#' # the edge attributes for that selection
#' graph %>% select_edges(from = c("b", "a"), to = c("c", "d")) %>%
#'   get_edge_attr_from_selection()
#' #>   from to        rel color
#' #> 1    a  d leading_to  pink
#' #> 2    b  c leading_to  blue
#' }
#' @export get_edge_attr_from_selection

get_edge_attr_from_selection <- function(graph,
                                         edge_attr,
                                         mode = NULL){

  if (is.null(graph$selection$edges)){
    stop("There is no selection of edges available.")
  }

  edges_df <- get_edge_attr(graph,
                            from = graph$selection$edges$from,
                            to = graph$selection$edges$to)

  if (is.null(edge_attr)){
    return(edges_df)
  }

  if (!is.null(edge_attr)){

    if (any(edge_attr %in% colnames(edges_df)[-c(1:2)])){

      edges_attr_vector <-
        edges_df[,which(colnames(edges_df) %in% edge_attr)]

      if (!is.null(mode)){
        if (mode == "numeric"){
          edges_attr_vector <- as.numeric(edges_attr_vector)

          edges_attr_vector <-
            edges_attr_vector[which(!is.na(edges_attr_vector))]
        }

        if (mode == "character"){
          edges_attr_vector <- as.character(edges_attr_vector)
        }
      }
    }

    graph$deposit <- edges_attr_vector

    return(graph)
  }
}
