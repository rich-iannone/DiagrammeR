#' Get node attributes based on a selection of nodes
#' @description From a graph object of class \code{dgr_graph}, get node
#' attribute properties for nodes available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param node_attr the node attribute from which to obtain values.
#' @param mode a option to recast the returned vector of node attribute
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
#' # Select nodes "a" and "c" in the graph and get the node
#' # attributes for that selection
#' graph %>% select_nodes(nodes = c("a", "c")) %>%
#'   get_node_attr_from_selection()
#' #>   nodes   type label value
#' #> 1     a letter     a   3.5
#' #> 3     c letter     c   9.4
#' }
#' @export get_node_attr_from_selection

get_node_attr_from_selection <- function(graph,
                                         node_attr,
                                         mode = NULL){

  if (is.null(graph$selection$nodes)){
    stop("There is no selection of nodes available.")
  }

  nodes_df <-
    get_node_df(graph)[which(get_node_df(graph)[,1]
                             %in% graph$selection$nodes),]

  if (any(node_attr %in% colnames(nodes_df)[-1])){

    nodes_attr_vector <-
      nodes_df[,which(colnames(nodes_df) %in% node_attr)]

    if (!is.null(mode)){
      if (mode == "numeric"){
        nodes_attr_vector <- as.numeric(nodes_attr_vector)

        nodes_attr_vector <-
          nodes_attr_vector[which(!is.na(nodes_attr_vector))]
      }

      if (mode == "character"){
        nodes_attr_vector <- as.character(nodes_attr_vector)
      }
    }


    # Place vector of node attributes as a deposit in the graph
    graph$deposit <- nodes_attr_vector

    return(graph)
  }
}
