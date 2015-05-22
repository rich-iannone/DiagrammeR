#' Perform query on graph object
#' With a graph object, perform a query to return a graph selection
#' @param graph a graph object of class \code{dgr_graph}.
#' @param query a query string
#' @return a graph selection object
#' @import stringr
#' @export query_graph

query_graph <- function(graph,
                        query){

  # Parse simple queries
  # query <- "(a) -> b"

  # Parse query string
  simple_relationship_nodes <-
    str_replace_all(unlist(str_split(query, "(->|--)")), " ", "")

  # Get the nodes in the traversal
  traversal_nodes <-
    str_extract(simple_relationship_nodes, "[a-z]")

  # Get the relationships in the traversal
  traversal_rels_directed <-
    str_detect(unlist(str_extract_all(query, "(->|--)")), ">")

  # Is there uniqueness amongst the traversed nodes?
  traversal_nodes_unique <-
    ifelse(length(unique(traversal_nodes)) == length(traversal_nodes),
           TRUE, FALSE)

  # Determine whether any element must be explicitly captured
  capture_element <-
    any(str_detect(simple_relationship_nodes,
                   paste0("\\(", simple_relationship_nodes, ".*?\\)")))

  # Determine which element must be captured
  if (capture_element){
    if (traversal_nodes_unique){

      element_to_capture <-
        which(str_detect(simple_relationship_nodes,
                         paste0("\\(", simple_relationship_nodes, ".*?\\)")))
    }

    all_nodes <- get_nodes(graph = graph)

    get_successors(graph = graph, node = get_nodes(graph = graph))

  }
}
