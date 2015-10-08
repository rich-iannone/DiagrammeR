#' Add edges to an existing graph object
#' @description With a graph object of class \code{dgr_graph}, add one or more
#' edges of specified types to nodes within the graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param edges_df an edge data frame that is created using \code{create_edges}.
#' @param from a vector of the outgoing nodes from which each edge is connected.
#' @param to a vector of the incoming nodes to which each edge is connected.
#' @param rel a string specifying the relationship between the
#' connected nodes.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a graph with two nodes
#' graph <- create_graph(create_nodes(nodes = c("a", "b")))
#'
#' # Add an edge between those nodes and attach a relationship to the edge
#' graph <- add_edges(graph, from = "a", to = "b",
#'                    rel = "to_get")
#'
#' # Examples of pipeable graph building using 'create_edges' with
#' # 'add_edges' in order to include values for the 'style' edge attribute
#' # (it modifies the style of the connecting line)
#'
#' library(magrittr)
#'
#' graph <- create_graph() %>%
#'   add_node("a") %>%
#'   add_node("b") %>%
#'   add_edges(create_edges(from = "a", to = "b",
#'                          style = "solid")) %>%
#'   add_node("c") %>%
#'   add_node("d") %>%
#'   add_edges(create_edges(from = "c", to = "d",
#'                          style = "dashed")) %>%
#'   add_node("e") %>%
#'   add_node("f") %>%
#'   add_edges(create_edges(from = "e", to = "f",
#'                          style = "dotted")) %>%
#'   add_node("g") %>%
#'   add_node("h") %>%
#'   add_edges(create_edges(from = "g", to = "h",
#'                          style = "bold"))
#' }
#' @export add_edges

add_edges <- function(graph,
                      edges_df = NULL,
                      from = NULL,
                      to = NULL,
                      rel = NULL){

  if (is_graph_empty(graph) == TRUE){
    stop("Edges cannot be added to an empty graph.")
  }

  # If an edge data frame is supplied, it will be used to add edges to
  # the graph's nodes
  if (!is.null(edges_df)){

    # Ensure that the appropriate columns specifying edges are present
    from_present <- "from" %in% colnames(edges_df)
    to_present <- "to" %in% colnames(edges_df)

    edges_df_valid <- from_present & to_present

    # Ensure that the nodes specified are in the graph object
    all_nodes_in_graph <- all(get_nodes(edges_df) %in% get_nodes(graph))

    edges_df_available <- ifelse(edges_df_valid & all_nodes_in_graph,
                                 TRUE, FALSE)

    # If not all the nodes specified in the edge data frame are in the
    # graph, stop the function
    if (edges_df_available == FALSE){
      stop("Not all nodes specified in the edge data frame are in the graph.")
    }

    # If the 'edges_df' component of the graph is not null, combine the
    # incoming edge data frame with the existing edge definitions in the
    # graph object
    if (!is.null(graph$edges_df)){

      combined_edges <- combine_edges(graph$edges_df,
                                      edges_df)

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = combined_edges,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      return(dgr_graph)
    }

    # If the 'edges_df' component of the graph is null, insert the
    # edge data frame into the graph object
    if (is.null(graph$edges_df)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = edges_df,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      return(dgr_graph)
    }
  }

  # If an edge between nodes is requested and that edge exists, stop function
  if (all(!is.null(from), !is.null(to),
      all(!is.na(get_edges(graph, return_type = "vector"))))){

    if (any(get_edges(graph)[[1]] == from &
            get_edges(graph)[[2]] == to)){
      stop("This edge already exists.")
    }
  }

  # If 'graph$edges_df' is null then use 'create_edges' to add an edge
  if (is.null(graph$edges_df)){

    # If a relationship is defined, add that in the 'create_edges' call
    if (!is.null(rel)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = create_edges(from = from,
                                             to = to,
                                             rel = rel),
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)
    }

    # If a relationship is not defined, use a simpler 'create_edges' call
    if (is.null(rel)){

      dgr_graph <-
        create_graph(nodes_df = graph$nodes_df,
                     edges_df = create_edges(from = from,
                                             to = to),
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      return(dgr_graph)
    }
  }

  # If 'graph$edges_df' is not null then use both 'combine_edges' and
  # 'create_edges' to add an edge
  if (!is.null(graph$edges_df)){

    # If a relationship is defined, add that in the 'create_edges' call
    if (!is.null(rel)){

      combined_edges <-
        combine_edges(graph$edges_df,
                      create_edges(from = from,
                                   to = to,
                                   rel = rel))
    }

    # If a relationship is not defined, use a simpler 'create_edges' call
    if (is.null(rel)){

      combined_edges <-
        combine_edges(graph$edges_df,
                      create_edges(from = from,
                                   to = to))
    }

    # Use the 'combined_edges' object in either case to create an updated graph
    dgr_graph <-
      create_graph(nodes_df = graph$nodes_df,
                   edges_df = combined_edges,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }

  return(dgr_graph)
}
